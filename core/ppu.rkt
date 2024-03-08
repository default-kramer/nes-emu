#lang typed/racket

(provide PPUState ppustate? compile-ppu)
(module+ internals
  (provide define-ppu))

(require "../util.rkt"
         "../ufx.rkt")

(module+ test (require typed/rackunit))

(define mask-pixel-index #x3F)
(define mask-nmi? #x40)
(define mask-frame-complete? #x80)

(define-syntax-rule (define-register WISH reg)
  (define-syntax reg
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! id value)
          (quasisyntax/loc stx (WISH #:set! id value))]
         [id
          (identifier? #'id)
          (quasisyntax/loc stx (WISH #:get id))])))))

(define-syntax-rule (define-registers WISH reg ...)
  (begin (define-register WISH reg) ...))

(define-syntax-rule (define-ppu ooo WISH
                      #:clock clock
                      #:reset reset
                      #:register-read register-read
                      #:register-write register-write)
  {begin
    (define-registers WISH
      scanline ; int16
      cycle ; int16
      ppumask ; 8 bits of flags
      ppuctrl ; 8 bits of flags
      ppustatus ; 3 bits of flags (and 5 unused bits)
      fine-x ; uint8
      ppu-data-buffer ; uint8
      clock-result ; Fixnum should be fine
      ;frame-complete? ; boolean
      ;nmi? ; boolean
      address-latch? ; boolean

      ; Background rendering
      bg-next-tile-lsb ; uint8
      bg-next-tile-msb ; uint8
      bg-next-tile-id ; uint8
      bg-next-tile-attrib ; uint8
      bg-shifter-pattern-lo ; uint16
      bg-shifter-pattern-hi ; uint16
      bg-shifter-attrib-lo ; uint16
      bg-shifter-attrib-hi ; uint16
      vram-addr ; loopy, uint16
      tram-addr ; loopy, uint16
      )
    #;(: ppu-read (-> Fixnum Fixnum Byte))
    (define-syntax-rule (ppu-read addr)
      ; implementers need to look at the mask
      (WISH #:ppu-read addr ppumask))
    #;(: ppu-write (-> Fixnum Byte Void))
    (define-syntax-rule (ppu-write addr val)
      (WISH #:ppu-write addr val))
    ; =================================
    ; do not use WISH beyond this point
    ; =================================

    (define-maskers
      [#b0000000000011111 /coarse-x]
      [#b0000001111100000 /coarse-y]
      [#b0000010000000000 /nametable-x]
      [#b0000100000000000 /nametable-y]
      [#b0111000000000000 /fine-y])

    ; Masks for PPUCTRL
    (define-maskers
      [#b10000000 /enable-nmi?]
      [#b00100000 /sprite-size?]
      [#b00010000 /pattern-background?]
      [#b00001000 /pattern-sprite?]
      [#b00000100 /increment-mode?]
      ; dumb names here to avoid clash with loopy nametable-x and y:
      [#b00000010 /nty?]
      [#b00000001 /ntx?])

    ; Masks for PPUMASK
    (define-maskers
      [#b00000001 /grayscale?]
      [#b00000010 /render-background-left?]
      [#b00000100 /render-sprites-left?]
      [#b00001000 /render-bg?]
      [#b00010000 /render-sprites?])

    ; Masks for PPUSTATUS
    (define-maskers
      [#b10000000 /vblank?]
      [#b01000000 /sprite-zero-hit?]
      [#b00100000 /sprite-overflow?])

    (define-syntax-rule (render-bg-or-sprites?)
      (has-any-flag? ppumask /render-bg? /render-sprites?))

    (define-syntax-rule (increment-scroll-x)
      (when (render-bg-or-sprites?)
        (let ([old-coarse-x (/coarse-x vram-addr #:shifted)])
          (if (ufx= 31 old-coarse-x)
              (begin (/coarse-x vram-addr #:set! 0)
                     (/nametable-x vram-addr #:set! (ufxnot (/nametable-x vram-addr #:shifted))))
              (begin (/coarse-x vram-addr #:set! (ufx+ 1 old-coarse-x)))))))

    (define-syntax-rule (increment-scroll-y)
      (when (render-bg-or-sprites?)
        (let ([old-fine-y : Fixnum (/fine-y vram-addr #:shifted)])
          (if (ufx< old-fine-y 7)
              (/fine-y vram-addr #:set! (ufx+ 1 old-fine-y))
              ; else
              (let ([old-coarse-y (/coarse-y vram-addr #:shifted)])
                (/fine-y vram-addr #:set! 0)
                (case old-coarse-y
                  [(29)
                   (begin (/coarse-y vram-addr #:set! 0)
                          (/nametable-y vram-addr #:set!
                                        (ufxnot (/nametable-y vram-addr #:shifted))))]
                  [(31)
                   (/coarse-y vram-addr #:set! 0)]
                  [else
                   (/coarse-y vram-addr #:set! (ufx+ 1 old-coarse-y))]))))))

    (define-syntax-rule (transfer-address-x)
      (when (render-bg-or-sprites?)
        (copy-bits #:from tram-addr #:to vram-addr
                   /nametable-x /coarse-x)))

    (define-syntax-rule (transfer-address-y)
      (when (render-bg-or-sprites?)
        (copy-bits #:from tram-addr #:to vram-addr
                   /fine-y /nametable-y /coarse-y)))

    (define-syntax-rule (load-background-shifters)
      (begin
        (SET! bg-shifter-pattern-lo (ufxior bg-next-tile-lsb
                                            (ufxand #xFF00 bg-shifter-pattern-lo)))
        (SET! bg-shifter-pattern-hi (ufxior bg-next-tile-msb
                                            (ufxand #xFF00 bg-shifter-pattern-hi)))
        (SET! bg-shifter-attrib-lo (ufxior (ufxand #xFF00 bg-shifter-attrib-lo)
                                           (if (ufx= 0 (ufxand 1 bg-next-tile-attrib))
                                               0
                                               #xFF)))
        (SET! bg-shifter-attrib-hi (ufxior (ufxand #xFF00 bg-shifter-attrib-hi)
                                           (if (ufx= 0 (ufxand 2 bg-next-tile-attrib))
                                               0
                                               #xFF)))))

    (define-syntax-rule (shift-left! id ooo)
      (begin (SET! id (ufxand #xFFFF (ufxlshift id 1)))
             ooo))

    (define-syntax-rule (update-shifters)
      (begin
        (when (has-any-flag? ppumask /render-bg?)
          (shift-left! bg-shifter-pattern-lo
                       bg-shifter-pattern-hi
                       bg-shifter-attrib-lo
                       bg-shifter-attrib-hi))))

    (define-syntax-rule (clock-main-loop)
      (when (or (and (ufx>= cycle 2)
                     (ufx< cycle 258))
                (and (ufx>= cycle 321)
                     (ufx< cycle 338)))
        (update-shifters)
        (case (ufxand #b111 (ufx- cycle 1))
          [(0) (cycle0)]
          [(2) (cycle2)]
          [(4) (cycle4)]
          [(6) (cycle6)]
          [(7) (increment-scroll-x)])))

    (define-syntax-rule (cycle0)
      (begin
        (load-background-shifters)
        (let ([addr (ufxior #x2000
                            (ufxand #xFFF vram-addr))])
          (SET! bg-next-tile-id (ppu-read addr)))))

    (define-syntax-rule (cycle2)
      (let* ([addr #x23C0]
             [addr (ufxior addr (/nametable-y vram-addr #:unshifted))]
             [addr (ufxior addr (/nametable-x vram-addr #:unshifted))]
             [addr (ufxior addr (ufxlshift (ufxrshift (/coarse-y vram-addr #:shifted)
                                                      2)
                                           3))]
             [addr (ufxior addr (ufxrshift (/coarse-x vram-addr #:shifted) 2))]
             [val (ppu-read addr)])
        (SET! bg-next-tile-attrib val)
        (when (not (ufx= 0 (ufxand 2 (/coarse-y vram-addr #:shifted))))
          (SET! bg-next-tile-attrib (ufxrshift bg-next-tile-attrib 4)))
        (when (not (ufx= 0 (ufxand 2 (/coarse-x vram-addr #:shifted))))
          (SET! bg-next-tile-attrib (ufxrshift bg-next-tile-attrib 2)))
        (SET! bg-next-tile-attrib (ufxand 3 bg-next-tile-attrib))))

    ; Cycles 4 and 6 are very similar, so they share this code
    (define-syntax-rule (cycle46 assignee zero-or-eight)
      (let* ([bg-offset (ufxand #x10 ppuctrl)]
             [bg-offset (ufxlshift bg-offset 8)]
             ; Here bg-offset will have a value of $0000 or $1000
             ; depending on whether bit 4 of PPUCTRL was set.
             ; So it only controls bit 12
             [tile-offset (ufxlshift bg-next-tile-id 4)]
             ; Here tile-offset is a single byte shifted 4.
             ; So it only controls bits 4-11
             [fine-offset (/fine-y vram-addr #:shifted)]
             [fine-offset (ufxior zero-or-eight fine-offset)]
             ; And fine-offset is just a 4-bit number,
             ; so we can just fxior them all together
             [addr (ufxior bg-offset (ufxior tile-offset fine-offset))])
        (SET! assignee (ppu-read addr))))

    (define-syntax-rule (cycle4)
      (cycle46 bg-next-tile-lsb 0))

    (define-syntax-rule (cycle6)
      (cycle46 bg-next-tile-msb 8))

    (define-syntax-rule (clock)
      (begin
        (set! clock-result 0)

        (when (and (ufx>= scanline -1)
                   (ufx< scanline 240))
          (when (and (ufx= 0 scanline)
                     (ufx= 0 cycle))
            ; OLC: "Odd Frame" cycle skip
            (set! cycle 1))
          (when (and (ufx= -1 scanline)
                     (ufx= 1 cycle))
            ; OLC: Effectively start of new frame, so clear vertical blank flag
            (/vblank? ppustatus #:set! 0))
          (clock-main-loop)
          (when (ufx= cycle 256)
            ; OLC: End of a visible scanline, so increment downwards...
            (increment-scroll-y))
          (when (ufx= cycle 257)
            ; OLC: ...and reset the x position
            (load-background-shifters)
            (transfer-address-x))
          (when (or (ufx= cycle 338)
                    (ufx= cycle 340))
            ; OLC: Superfluous reads of tile id at end of scanline
            (SET! bg-next-tile-id (ppu-read (ufxior #x2000 (ufxand #xFFF vram-addr)))))
          (when (and (ufx= scanline -1)
                     (ufx>= cycle 280)
                     (ufx< cycle 305))
            ; OLC: End of vertical blank period so reset the Y address ready for rendering
            (transfer-address-y)))

        (when (and (ufx= 241 scanline)
                   (ufx= 1 cycle))
          ; End of frame
          (/vblank? ppustatus #:set! 1)
          (when (has-any-flag? ppuctrl /enable-nmi?)
            (set! clock-result (ufxior mask-nmi? clock-result))
            #;(SET! nmi? #t)))
    
        ; Compose the pixel!
        (let ([bg-pixel : Byte 0]
              [bg-palette : Byte 0]
              [pixel-index : Byte 0])
          (when (has-any-flag? ppumask /render-bg?)
            (let* ([bit-mux (ufxrshift #x8000 fine-x)]
                   [p0-pixel (if (ufx= 0 (ufxand bg-shifter-pattern-lo bit-mux))
                                 0
                                 1)]
                   [p1-pixel (if (ufx= 0 (ufxand bg-shifter-pattern-hi bit-mux))
                                 0
                                 2)]
                   [bg-pal0 (if (ufx= 0 (ufxand bg-shifter-attrib-lo bit-mux))
                                0
                                1)]
                   [bg-pal1 (if (ufx= 0 (ufxand bg-shifter-attrib-hi bit-mux))
                                0
                                2)])
              (set! bg-pixel (ufxior p0-pixel p1-pixel))
              (set! bg-palette (ufxior bg-pal0 bg-pal1))))
          ; Be careful - this ppu-read (likely) needs to happen on every cycle:
          (define addr (ufxior #x3F00 (ufxior bg-pixel (ufxlshift bg-palette 2))))
          (set! pixel-index (ufxand mask-pixel-index (ppu-read addr)))
          ; stash pixel-index into clock-result
          (set! clock-result (ufxior clock-result pixel-index)))

        (define return-x (ufx+ cycle -1))
        (define return-y scanline)

        ; Advance renderer
        (set! cycle (ufx+ 1 cycle))
        (when (ufx>= cycle 341)
          (set! cycle 0)
          (set! scanline (ufx+ 1 scanline))
          (when (ufx>= scanline 261)
            (set! scanline -1)
            (set! clock-result (ufxior mask-frame-complete? clock-result))
            #;(set! frame-complete? #t)))

        ; Return
        (values clock-result return-x return-y)))

    (define-syntax-rule (reset)
      (begin (SET! fine-x 0)
             (SET! ppu-data-buffer 0)
             (SET! scanline 0)
             (SET! cycle 0)
             (SET! bg-next-tile-id 0)
             (SET! bg-next-tile-attrib 0)
             (SET! bg-next-tile-lsb 0)
             (SET! bg-next-tile-msb 0)
             (SET! bg-shifter-pattern-lo 0)
             (SET! bg-shifter-pattern-hi 0)
             (SET! bg-shifter-attrib-lo 0)
             (SET! bg-shifter-attrib-hi 0)
             (SET! ppustatus 0)
             (SET! ppumask 0)
             (SET! ppuctrl 0)
             (SET! vram-addr 0)
             (SET! tram-addr 0)))

    (define-syntax-rule (increment-vram-addr)
      (let ([increment (if (has-any-flag? ppuctrl /increment-mode?)
                           32
                           1)])
        (SET! vram-addr (ufx+ increment vram-addr))))


    ; see olc2C02::cpuWrite
    ; see https://www.nesdev.org/wiki/PPU_registers
    (define-syntax-rule (register-write addr in-value)
      (let ([value in-value])
        (case (ufxand 7 addr)
          [(0)
           (SET! ppuctrl value)
           (/ntx? ppuctrl #:set! (/nametable-x tram-addr #:shifted))
           (/nty? ppuctrl #:set! (/nametable-y tram-addr #:shifted))]
          [(1)
           (SET! ppumask value)]
          [(2) ; ppustatus is not writable
           (void)]
          [(3)
           #;(error "TODO write OAM Address")
           (void)]
          [(4)
           #;(error "TODO write OAM Data")
           (void)]
          [(5) ; PPUSCROLL
           (if (not address-latch?)
               (begin (SET! fine-x (ufxand 7 value))
                      (/coarse-x tram-addr #:set! (ufxrshift value 3))
                      (SET! address-latch? #t))
               (begin (/fine-y tram-addr #:set! (ufxand 7 value))
                      (/coarse-y tram-addr #:set! (ufxrshift value 3))
                      (SET! address-latch? #f)))]
          [(6)
           (if (not address-latch?)
               (begin (SET! tram-addr (ufxior (ufxand #xFF tram-addr)
                                              (ufxlshift (ufxand #x3F value) 8)))
                      (SET! address-latch? #t)
                      #;(println (list "6.1" value vram-addr tram-addr)))
               (begin (SET! tram-addr (ufxior value (ufxand #xFF00 tram-addr)))
                      (SET! vram-addr tram-addr)
                      (SET! address-latch? #f)
                      #;(println (list "6.2" value vram-addr tram-addr))))]
          [(7)
           ; Note that ppu-write can depend on the mapper/cartridge
           (ppu-write vram-addr value)
           (increment-vram-addr)])
        (void)))

    (define-syntax-rule (register-read addr)
      (case (ufxand 7 addr)
        [(2)
         (let ([result : Byte (ufxior (ufxand #xE0 ppustatus)
                                      (ufxand #x1F ppu-data-buffer))])
           (/vblank? ppustatus #:set! 0)
           (SET! address-latch? #f)
           result)]
        [(7)
         ; OLC: Reads from the NameTable ram get delayed one cycle
         (let ([result : Byte ppu-data-buffer])
           (SET! ppu-data-buffer (ppu-read vram-addr))
           ; OLC: However, if the address was in the palette range, the data is not delayed, so it returns immediately
           (when (ufx>= vram-addr #x3F00)
             (set! result ppu-data-buffer))
           (increment-vram-addr)
           result)]
        [else
         0]))
    })

(define-syntax-rule (define-ppu-compiler [#:compile-ppu compile-ppu #:state-type StateType]
                      [var-id :: var-type var-init-val] ...)
  (begin
    (define-type StateType (List (Pair 'var-id var-type) ...))
    (define-syntax-rule (compile-ppu #:ooo ooo #:wish WISH)
      ; PERFORMANCE NOTE! The approach that follows returns clock/reset procedures
      ; which close over the let-bound state variables.
      ; At first I thought this approach was slower than using top-level definitions
      ; for the procedures and the state variables.
      ; But it turns out that using top-level definitions is actually slower!
      ; I think my top-level code was, at first, "cheating" by defining the
      ; ppu-read and ppu-write procedures in the same module.
      ; Since this "cheating" will be difficult/impossible to do in real emulation,
      ; I don't care too much about losing that performance.
      ; For the record, here is what I observed for a 600-frame canned PPU-only test:
      ;   top-level, cheating: 312 millis
      ;   let-bound, nocheat : 484 millis
      ;   top-level, nocheat : 625 millis
      ;   mutable struct     : 1437 millis
      ; So for now I think the following approach (let-bound, nocheat) is the best I can do.
      (let ([var-id :: var-type var-init-val]
            ...)
        (define-syntax (wish stx)
          (syntax-case stx ()
            [(_ #:get foo)
             (equal? (syntax-e #'foo) 'var-id)
             (syntax/loc stx var-id)]
            ...
            [(_ #:set! foo val)
             (equal? (syntax-e #'foo) 'var-id)
             (syntax/loc stx (set! var-id val))]
            ...
            [(_ stuff ooo)
             (syntax/loc stx (WISH stuff ooo))]))
        (define-ppu ooo wish #:clock clock-macro #:reset reset-macro
          #:register-read register-read-macro #:register-write register-write-macro)
        (: clock (-> (Values Fixnum Fixnum Fixnum)))
        (define (clock) (clock-macro))
        (: reset (-> Void))
        (define (reset) (reset-macro))
        (: get-state (-> StateType))
        (define (get-state) (list (cons 'var-id var-id) ...))
        (: register-read (-> Fixnum Byte))
        (define (register-read addr) (register-read-macro addr))
        (: register-write (-> Fixnum Byte Void))
        (define (register-write addr value) (register-write-macro addr value))
        (values clock reset get-state register-read register-write)))))

(define-ppu-compiler [#:compile-ppu compile-ppu-raw #:state-type PPUState]
  [scanline : Fixnum 0] ; int16
  [cycle : Fixnum 0] ; int16
  [ppumask : Fixnum 0] ; 8 bits of flags
  [ppuctrl : Fixnum 0] ; 8 bits of flags
  [ppustatus : Fixnum 0] ; 3 bits of flags (and 5 unused bits)
  [fine-x : Fixnum 0] ; uint8
  [ppu-data-buffer : Byte 0] ; uint8
  ;[frame-complete? : Boolean #f] ; boolean
  ;[nmi? : Boolean #f] ; boolean
  [clock-result : Fixnum 0]
  [address-latch? : Boolean #f] ; boolean
  ; Background rendering
  [bg-next-tile-lsb : Fixnum 0] ; uint8
  [bg-next-tile-msb : Fixnum 0] ; uint8
  [bg-next-tile-id : Fixnum 0] ; uint8
  [bg-next-tile-attrib : Fixnum 0] ; uint8
  [bg-shifter-pattern-lo : Fixnum 0] ; uint16
  [bg-shifter-pattern-hi : Fixnum 0] ; uint16
  [bg-shifter-attrib-lo : Fixnum 0] ; uint16
  [bg-shifter-attrib-hi : Fixnum 0] ; uint16
  [vram-addr : Fixnum 0] ; loopy, uint16
  [tram-addr : Fixnum 0])

(define ppustate? (make-predicate PPUState))

(define-syntax (compile-ppu stx)
  (syntax-case stx ()
    [(_ stuff ...)
     (with-syntax ([ooo (quote-syntax ...)])
       (syntax/loc stx
         (compile-ppu-raw #:ooo ooo stuff ...)))]))

(module+ test
  ; Alert if I accidentally change the type definition of PPUState:
  (check-true (ppustate? '((scanline . 0)
                           (cycle . 0)
                           (ppumask . 0)
                           (ppuctrl . 0)
                           (ppustatus . 0)
                           (fine-x . 0)
                           (ppu-data-buffer . 0)
                           ;(frame-complete? . #f)
                           ;(nmi? . #f)
                           (clock-result . 0)
                           (address-latch? . #f)
                           (bg-next-tile-lsb . 0)
                           (bg-next-tile-msb . 0)
                           (bg-next-tile-id . 0)
                           (bg-next-tile-attrib . 0)
                           (bg-shifter-pattern-lo . 0)
                           (bg-shifter-pattern-hi . 0)
                           (bg-shifter-attrib-lo . 0)
                           (bg-shifter-attrib-hi . 0)
                           (vram-addr . 0)
                           (tram-addr . 0)))))

(module+ test
  (define-syntax (wish stx)
    (syntax-case stx ()
      [(_ #:ppu-read addr ppumask)
       #'(ann 0 Byte)]
      [(_ #:ppu-write addr val)
       #'(error "TODO ppu-write")]
      [(_ #:compose-pixel bg-palette bg-pixel)
       #'(void "TODO compose-pixel")]
      [else (raise-syntax-error #f "unrecognized wish" stx)]))
  (define-values (clock reset get-state register-read register-write)
    (compile-ppu #:wish wish))
  (reset)
  ;(println (get-state))
  (clock)
  )
