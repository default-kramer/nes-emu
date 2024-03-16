#lang typed/racket

(provide PPUState (struct-out ppustate) compile-ppu)
(module+ internals
  (provide define-ppu))

(require racket/unsafe/ops
         "../util.rkt"
         "../ufx.rkt")

(module+ test (require typed/rackunit))

(define-syntax-rule (assert condition)
  (begin
    #;(when (not condition)
        (error "Assert failed:" 'condition))
    (void)))

(struct sprite-info ([y : Byte]
                     [id : Byte]
                     [attribute : Byte]
                     [x : Byte])
  #:mutable)


; Every Sprite-Info-Table must have 72 entries, 64 for OAM + 8 for internal use
(define-type Sprite-Info-Table (Immutable-Vectorof sprite-info))
(define (make-sprite-info-table)
  (let* ([count 72]
         [items (build-list count (lambda (i) (sprite-info 0 0 0 0)))]
         [vec (apply vector-immutable items)])
    (ann vec Sprite-Info-Table)))

(define mask-pixel-index #x3F)
(define mask-nmi? #x40)
(define mask-frame-complete? #x80)

; for/fixnum - because Typed Racket seems to struggle with:
#;(for ([i : Fixnum (in-range (ann 5 Fixnum))])
    (println i))
(define-syntax-rule (for/fixnum ([i #:from start #:until end]) body ...)
  (let loop ([i : Fixnum start])
    (when (ufx< i end)
      body ...
      (loop (ufx+ 1 i)))))

(define-syntax-rule (flip-byte b)
  (let* ([x (ann b Byte)]
         [x (ufxior (ufxrshift (ufxand x #xF0) 4)
                    (ufxlshift (ufxand x #x0F) 4))]
         [x (ufxior (ufxrshift (ufxand x #xCC) 2)
                    (ufxlshift (ufxand x #x33) 2))]
         [x (ufxior (ufxrshift (ufxand x #xAA) 1)
                    (ufxlshift (ufxand x #x55) 1))])
    (ann (ufxand 255 x) Byte)))
(module+ test
  (check-equal? (flip-byte #b10110010) #b01001101)
  (check-equal? (flip-byte 255) 255)
  (check-equal? (flip-byte 0) 0))

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
                      #:register-write register-write
                      #:dma-write dma-write)
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
      oam-addr ; uint8

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

      ; Foreground
      sprite-count ; uint8, should never exceed 8
      sprite-zero-hit-possible? ; bool
      sprite-zero-being-rendered? ; bool
      )
    #;(: ppu-read (-> Fixnum Fixnum Byte))
    (define-syntax-rule (ppu-read addr)
      ; implementers need to look at the mask
      (WISH #:ppu-read addr ppumask))
    #;(: ppu-write (-> Fixnum Byte Void))
    (define-syntax-rule (ppu-write addr val)
      (WISH #:ppu-write addr val))
    (define-syntax-rule (get-sprite-table)
      (ann (WISH #:get-sprite-table) Sprite-Info-Table))
    (define-syntax-rule (get-sprite-shifter i)
      (ann (WISH #:get-sprite-shifter i) Byte))
    (define-syntax-rule (set-sprite-shifter! i val)
      (WISH #:set-sprite-shifter i (ann val Byte)))
    ; =================================
    ; do not use WISH beyond this point
    ; =================================
    (define-syntax-rule (get-sprite-info index)
      (begin
        (assert (and (ufx>= index 0)
                     (ufx<= index 63)))
        (unsafe-vector-ref (get-sprite-table) index)))
    (define-syntax-rule (get-internal-sprite-info index)
      ; The first 64 are OAM, followed by 8 internal
      (begin
        (let ([i (ufxior 64 index)])
          (assert (and (ufx>= i 64)
                       (ufx< i 72)))
          (unsafe-vector-ref (get-sprite-table) i))))
    (define-syntax-rule (get-sprite-shifter-lo i)
      (begin
        (assert (and (ufx>= i 0)
                     (ufx<= i 7)))
        (get-sprite-shifter i)))
    (define-syntax-rule (get-sprite-shifter-hi i)
      (begin
        (assert (and (ufx>= i 0)
                     (ufx<= i 7)))
        (get-sprite-shifter (ufxior 8 i))))
    (define-syntax-rule (set-sprite-shifter-lo! i val)
      (begin
        (assert (and (ufx>= i 0)
                     (ufx<= i 7)))
        (set-sprite-shifter! i val)))
    (define-syntax-rule (set-sprite-shifter-hi! i val)
      (begin
        (assert (and (ufx>= i 0)
                     (ufx<= i 7)))
        (set-sprite-shifter! (ufxior 8 i) val)))

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
        (set! bg-shifter-pattern-lo (ufxior bg-next-tile-lsb
                                            (ufxand #xFF00 bg-shifter-pattern-lo)))
        (set! bg-shifter-pattern-hi (ufxior bg-next-tile-msb
                                            (ufxand #xFF00 bg-shifter-pattern-hi)))
        (set! bg-shifter-attrib-lo (ufxior (ufxand #xFF00 bg-shifter-attrib-lo)
                                           (if (ufx= 0 (ufxand 1 bg-next-tile-attrib))
                                               0
                                               #xFF)))
        (set! bg-shifter-attrib-hi (ufxior (ufxand #xFF00 bg-shifter-attrib-hi)
                                           (if (ufx= 0 (ufxand 2 bg-next-tile-attrib))
                                               0
                                               #xFF)))))

    (define-syntax-rule (shift-left! id ooo)
      (begin (set! id (ufxand #xFFFF (ufxlshift id 1)))
             ooo))

    (define-syntax-rule (update-shifters)
      (begin
        (when (has-any-flag? ppumask /render-bg?)
          (shift-left! bg-shifter-pattern-lo
                       bg-shifter-pattern-hi
                       bg-shifter-attrib-lo
                       bg-shifter-attrib-hi))
        (when (and (ufx>= cycle 1)
                   (ufx< cycle 258)
                   (has-any-flag? ppumask /render-sprites?))
          (for/fixnum ([i #:from 0 #:until sprite-count])
            (assert (ufx< i 8))
            (let* ([internal-sprite (get-internal-sprite-info i)]
                   [sprite-x : Byte (sprite-info-x internal-sprite)])
              (if (ufx> sprite-x 0)
                  (let ([sprite-x : Byte (ufx- sprite-x 1)])
                    (set-sprite-info-x! internal-sprite sprite-x))
                  (let ([lo (get-sprite-shifter-lo i)]
                        [hi (get-sprite-shifter-hi i)])
                    (set-sprite-shifter-lo! i (ufxand 255 (ufxlshift lo 1)))
                    (set-sprite-shifter-hi! i (ufxand 255 (ufxlshift hi 1))))))))))

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
        (let* ([addr (ufxior #x2000
                             (ufxand #xFFF vram-addr))]
               [value (ppu-read addr)])
          (set! bg-next-tile-id value))))

    (define-syntax-rule (cycle2)
      (let* ([addr #x23C0]
             [addr (ufxior addr (/nametable-y vram-addr #:unshifted))]
             [addr (ufxior addr (/nametable-x vram-addr #:unshifted))]
             [addr (ufxior addr (ufxlshift (ufxrshift (/coarse-y vram-addr #:shifted)
                                                      2)
                                           3))]
             [addr (ufxior addr (ufxrshift (/coarse-x vram-addr #:shifted) 2))]
             [val (ppu-read addr)])
        (set! bg-next-tile-attrib val)
        (when (not (ufx= 0 (ufxand 2 (/coarse-y vram-addr #:shifted))))
          (set! bg-next-tile-attrib (ufxrshift bg-next-tile-attrib 4)))
        (when (not (ufx= 0 (ufxand 2 (/coarse-x vram-addr #:shifted))))
          (set! bg-next-tile-attrib (ufxrshift bg-next-tile-attrib 2)))
        (set! bg-next-tile-attrib (ufxand 3 bg-next-tile-attrib))))

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
        (set! assignee (ppu-read addr))))

    (define-syntax-rule (cycle4)
      (cycle46 bg-next-tile-lsb 0))

    (define-syntax-rule (cycle6)
      (cycle46 bg-next-tile-msb 8))

    (define-syntax-rule (clear-sprite-shifters)
      (for ([i : Fixnum '(0 1 2 3 4 5 6 7)])
        (set-sprite-shifter-lo! i 0)
        (set-sprite-shifter-hi! i 0)))

    (define-syntax-rule (evaluate-sprites)
      {begin
        (set! sprite-count 0)
        (clear-sprite-shifters)
        (set! sprite-zero-hit-possible? #f)
        (let ([sprite-size (if (ufx= 0 (/sprite-size? ppuctrl #:unshifted)) 8 16)])
          ; Note - OLC definitely has some bugs here which I attempt to fix.
          ; This loop will allow sprite-count to go to 9 to detect sprite overflow,
          ; but we will immediately clamp it back down to 8 if that happens.
          (let loop ([oam-index : Fixnum 0])
            (when (and (ufx< oam-index 64)
                       (ufx< sprite-count 9))
              (let* ([entry (get-sprite-info oam-index)]
                     [diff (ufx- scanline (sprite-info-y entry))])
                (when (and (ufx>= diff 0)
                           (ufx< diff sprite-size))
                  (when (ufx< sprite-count 8)
                    (when (ufx= 0 oam-index)
                      (set! sprite-zero-hit-possible? #t))
                    (let ([internal (get-internal-sprite-info sprite-count)])
                      ; Copy from OAM into internal
                      (set-sprite-info-y!         internal (sprite-info-y         entry))
                      (set-sprite-info-id!        internal (sprite-info-id        entry))
                      (set-sprite-info-attribute! internal (sprite-info-attribute entry))
                      (set-sprite-info-x!         internal (sprite-info-x         entry))))
                  (set! sprite-count (ufx+ 1 sprite-count)))
                (loop (ufx+ 1 oam-index))))))
        (if (ufx<= sprite-count 8)
            (begin
              (/sprite-overflow? ppustatus #:set! 0)
              ; The unused internal entries need to be deactivated.
              ; Wait, no... as long as we have an accurate sprite-count
              ; who cares what is in the extraneous entries?
              #;(for/fixnum ([i #:from sprite-count #:until 8])
                  (let ([si (get-internal-sprite-info i)])
                    (set-sprite-info-y! si 255))))
            (begin
              (/sprite-overflow? ppustatus #:set! 1)
              (set! sprite-count 8)))
        })

    (define-syntax-rule (prepare-sprite-shifters)
      {begin
        (define is-8x8? (ufx= 0 (/sprite-size? ppuctrl #:unshifted)))
        (for/fixnum ([i #:from 0 #:until sprite-count])
          (assert (ufx< i 8))
          (define sprite (get-internal-sprite-info i))
          (define sprite-id (sprite-info-id sprite))
          (define sprite-y (sprite-info-y sprite))
          (define sprite-attr (sprite-info-attribute sprite))
          (define flip-horizontal? (ufx= #x40 (ufxand #x40 sprite-attr)))
          (define flip-vertical? (ufx= #x80 (ufxand #x80 sprite-attr)))
          (define sprite-pattern-addr-lo : Fixnum
            (if is-8x8?
                (let ([base (ufxior (ufxlshift (/pattern-sprite? ppuctrl #:shifted) 12)
                                    (ufxlshift sprite-id 4))]
                      [row (ufx- scanline sprite-y)])
                  (if flip-vertical?
                      (ufxior base (ufx- 7 row))
                      (ufxior base row)))
                ; else we are in 8x16 mode:
                (let* ([pattern (ufxlshift (ufxand 1 sprite-id) 12)]
                       [row (ufx- scanline sprite-y)]
                       [cell (ufxlshift (ufxior (if (ufx< row 8) 0 1)
                                                (ufxand #xFE sprite-id))
                                        4)]
                       [row (ufxand 7 row)])
                  (if flip-vertical?
                      (ufxior* pattern cell row (ufx- 7 row))
                      (ufxior* pattern cell row)))))
          ; OLC: Hi bit plane equivalent is always offset by 8 bytes from lo bit plane
          (define sprite-pattern-addr-hi (ufx+ 8 sprite-pattern-addr-lo))
          (let* ([sprite-pattern-bits-lo : Byte (ppu-read sprite-pattern-addr-lo)]
                 [sprite-pattern-bits-hi : Byte (ppu-read sprite-pattern-addr-hi)]
                 [sprite-pattern-bits-lo (if flip-horizontal?
                                             (flip-byte sprite-pattern-bits-lo)
                                             sprite-pattern-bits-lo)]
                 [sprite-pattern-bits-hi (if flip-horizontal?
                                             (flip-byte sprite-pattern-bits-hi)
                                             sprite-pattern-bits-hi)])
            (set-sprite-shifter-lo! i sprite-pattern-bits-lo)
            (set-sprite-shifter-hi! i sprite-pattern-bits-hi)))
        })

    (define-syntax-rule (calc-bg-pixel+palette)
      {begin
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
          (values (ufxior p0-pixel p1-pixel)
                  (ufxior bg-pal0 bg-pal1)))
        })

    (define-syntax-rule (calc-fg-pixel+palette+priority)
      {begin
        (set! sprite-zero-being-rendered? #f)
        (let ([pixel : Fixnum 0]
              [palette : Fixnum 0]
              [priority? : Boolean #f])
          (for/fixnum ([i #:from 0 #:until sprite-count])
            (assert (ufx< i 8))
            (define sprite (get-internal-sprite-info i))
            (when (ufx= 0 (sprite-info-x sprite))
              (let* ([pixel-lo (ufxrshift (ufxand #x80 (get-sprite-shifter-lo i)) 7)]
                     [pixel-hi (ufxrshift (ufxand #x80 (get-sprite-shifter-hi i)) 6)]
                     [attr (sprite-info-attribute sprite)])
                (set! pixel (ufxior pixel-lo pixel-hi))
                (set! palette (ufx+ 4 (ufxand 3 attr)))
                (set! priority? (ufx= 0 (ufxand #x20 attr)))
                (when (not (ufx= 0 pixel))
                  (when (ufx= i 0)
                    (set! sprite-zero-being-rendered? #t))
                  (set! i 999) ; exit loop
                  ))))
          (values pixel palette priority?))
        })

    (define-syntax-rule (clock)
      (begin
        (set! clock-result 0)

        ; Advance renderer
        (set! cycle (ufx+ 1 cycle))
        (when (ufx>= cycle 341)
          (set! cycle 0)
          (set! scanline (ufx+ 1 scanline))
          (when (ufx>= scanline 261)
            (set! scanline -1)
            (set! clock-result (ufxior mask-frame-complete? clock-result))))

        (when (and (ufx>= scanline -1)
                   (ufx< scanline 240))
          (when (and (ufx= 0 scanline)
                     (ufx= 0 cycle))
            ; OLC: "Odd Frame" cycle skip
            (set! cycle 1))
          (when (and (ufx= -1 scanline)
                     (ufx= 1 cycle))
            ; OLC: Effectively start of new frame, so clear vertical blank flag
            (/vblank? ppustatus #:set! 0)
            (/sprite-overflow? ppustatus #:set! 0)
            (/sprite-zero-hit? ppustatus #:set! 0)
            (clear-sprite-shifters))
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
            (set! bg-next-tile-id (ppu-read (ufxior #x2000 (ufxand #xFFF vram-addr)))))
          (when (and (ufx= scanline -1)
                     (ufx>= cycle 280)
                     (ufx< cycle 305))
            ; OLC: End of vertical blank period so reset the Y address ready for rendering
            (transfer-address-y))
          (when (and (ufx= cycle 257)
                     (ufx>= scanline 0))
            (evaluate-sprites))
          (when (ufx= cycle 340)
            (prepare-sprite-shifters)))

        (when (and (ufx= 241 scanline)
                   (ufx= 1 cycle))
          ; End of frame
          (/vblank? ppustatus #:set! 1)
          (when (has-any-flag? ppuctrl /enable-nmi?)
            (set! clock-result (ufxior mask-nmi? clock-result))))
    
        ; Compose the pixel!
        ; If foreground wins, can we avoid background computations?
        ; NOPE because we need to do collision detection.
        (define-values (pixel palette)
          (let-values ([(fg-pixel fg-palette fg-priority?)
                        (if (has-any-flag? ppumask /render-sprites?)
                            (calc-fg-pixel+palette+priority)
                            (values 0 0 #f))]
                       [(bg-pixel bg-palette)
                        (if (has-any-flag? ppumask /render-bg?)
                            (calc-bg-pixel+palette)
                            (values 0 0))])
            (cond
              [(and (ufx= 0 bg-pixel)
                    (ufx= 0 fg-pixel))
               (values 0 0)]
              [(and (ufx= 0 bg-pixel)
                    (ufx> fg-pixel 0))
               (values fg-pixel fg-palette)]
              [(and (ufx> bg-pixel 0)
                    (ufx= 0 fg-pixel))
               (values bg-pixel bg-palette)]
              [else
               (begin
                 (when (and sprite-zero-hit-possible?
                            sprite-zero-being-rendered?
                            (has-any-flag? ppumask /render-bg?)
                            (has-any-flag? ppumask /render-sprites?))
                   (if (has-any-flag? ppumask /render-background-left? /render-sprites-left?)
                       (when (and (ufx>= cycle 1) (ufx< cycle 258))
                         (/sprite-zero-hit? ppustatus #:set! 1))
                       (when (and (ufx>= cycle 9) (ufx< cycle 258))
                         (/sprite-zero-hit? ppustatus #:set! 1))))
                 (if fg-priority?
                     (values fg-pixel fg-palette)
                     (values bg-pixel bg-palette)))])))
        ; Okay, now we have the "winning" pixel and palette
        ; Be careful - this ppu-read (likely) needs to happen on every cycle:
        (let* ([addr (ufxior #x3F00 (ufxior pixel (ufxlshift palette 2)))]
               [pixel-index (ufxand mask-pixel-index (ppu-read addr))])
          ; stash pixel-index into clock-result
          (set! clock-result (ufxior clock-result pixel-index)))

        (define return-x (ufx+ cycle -1))
        (define return-y scanline)

        ; Return
        (values clock-result return-x return-y)))

    (define-syntax-rule (reset)
      (begin (set! fine-x 0)
             (set! ppu-data-buffer 0)
             (set! scanline 0)
             (set! cycle -1)
             (set! bg-next-tile-id 0)
             (set! bg-next-tile-attrib 0)
             (set! bg-next-tile-lsb 0)
             (set! bg-next-tile-msb 0)
             (set! bg-shifter-pattern-lo 0)
             (set! bg-shifter-pattern-hi 0)
             (set! bg-shifter-attrib-lo 0)
             (set! bg-shifter-attrib-hi 0)
             (set! ppustatus 0)
             (set! ppumask 0)
             (set! ppuctrl 0)
             (set! vram-addr 0)
             (set! tram-addr 0)))

    (define-syntax-rule (increment-vram-addr)
      (let ([increment (if (has-any-flag? ppuctrl /increment-mode?)
                           32
                           1)])
        (set! vram-addr (ufx+ increment vram-addr))))


    ; see olc2C02::cpuWrite
    ; see https://www.nesdev.org/wiki/PPU_registers
    (define-syntax-rule (register-write addr in-value)
      (let ([value in-value])
        (case (ufxand 7 addr)
          [(0)
           (set! ppuctrl value)
           (/nametable-x tram-addr #:set! (/ntx? ppuctrl #:shifted))
           (/nametable-y tram-addr #:set! (/nty? ppuctrl #:shifted))]
          [(1)
           (set! ppumask value)]
          [(2) ; ppustatus is not writable
           (void)]
          [(3)
           (set! oam-addr in-value)]
          [(4)
           (dma-write oam-addr in-value)]
          [(5) ; PPUSCROLL
           (if (not address-latch?)
               (begin (set! fine-x (ufxand 7 value))
                      (/coarse-x tram-addr #:set! (ufxrshift value 3))
                      (set! address-latch? #t))
               (begin (/fine-y tram-addr #:set! (ufxand 7 value))
                      (/coarse-y tram-addr #:set! (ufxrshift value 3))
                      (set! address-latch? #f)))]
          [(6)
           (if (not address-latch?)
               (begin (set! tram-addr (ufxior (ufxand #xFF tram-addr)
                                              (ufxlshift (ufxand #x3F value) 8)))
                      (set! address-latch? #t)
                      #;(println (list "6.1" value vram-addr tram-addr)))
               (begin (set! tram-addr (ufxior value (ufxand #xFF00 tram-addr)))
                      (set! vram-addr tram-addr)
                      (set! address-latch? #f)
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
           (set! address-latch? #f)
           result)]
        [(7)
         ; OLC: Reads from the NameTable ram get delayed one cycle
         (let ([result : Byte ppu-data-buffer])
           (set! ppu-data-buffer (ppu-read vram-addr))
           ; OLC: However, if the address was in the palette range, the data is not delayed, so it returns immediately
           (when (ufx>= vram-addr #x3F00)
             (set! result ppu-data-buffer))
           (increment-vram-addr)
           result)]
        [else
         0]))

    (define-syntax-rule (dma-write addr data)
      ; This is slightly awkward because DMA writes a single byte at a time.
      ; OAM has 64 entries at 4 bytes per entry, so
      ; * the highest 6 bits choose which of the 64 entries
      ; * the lowest 2 bits choose which field of that entry
      (let* ([index (ufxrshift (ufxand addr 255) 2)]
             ; assert 0 <= index <= 63
             [field (ufxand addr 3)]
             ; assert 0 <= field <= 3
             [sprite (get-sprite-info index)]
             [val (ann data Byte)])
        (case field
          [(0) ; y id attribute x
           (set-sprite-info-y! sprite val)]
          [(1)
           (set-sprite-info-id! sprite val)]
          [(2)
           (set-sprite-info-attribute! sprite val)]
          [(3)
           (set-sprite-info-x! sprite val)])))
    })

(define-syntax-rule (define-ppu-compiler [#:compile-ppu compile-ppu #:state-struct state-struct #:state-type StateType]
                      [var-id :: var-type var-init-val] ...)
  (begin
    (struct state-struct ([var-id :: var-type] ...)
      #:transparent #:type-name StateType)
    #;(define-type StateType (List (Pair 'var-id var-type) ...))
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
            ...
            ; WARNING - The following will need to be included in StateType and get-state
            ; if we want to use those for savestate functionality...
            [sprite-info-table (make-sprite-info-table)]
            [sprite-shifters (make-bytes 16)])
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
            [(_ #:get-sprite-table)
             (syntax/loc stx sprite-info-table)]
            [(_ #:get-sprite-shifter i)
             (syntax/loc stx (unsafe-bytes-ref sprite-shifters i))]
            [(_ #:set-sprite-shifter i val)
             (syntax/loc stx (unsafe-bytes-set! sprite-shifters i val))]
            [(_ stuff ooo)
             (syntax/loc stx (WISH stuff ooo))]))
        (define-ppu ooo wish #:clock clock-macro #:reset reset-macro
          #:register-read register-read-macro #:register-write register-write-macro
          #:dma-write dma-write-macro)
        (: clock (-> (Values Fixnum Fixnum Fixnum)))
        (define (clock) (clock-macro))
        (: reset (-> Void))
        (define (reset) (reset-macro))
        (: get-state (-> StateType))
        (define (get-state) (state-struct var-id ...))
        (: register-read (-> Fixnum Byte))
        (define (register-read addr) (register-read-macro addr))
        (: register-write (-> Fixnum Byte Void))
        (define (register-write addr value) (register-write-macro addr value))
        (: dma-write (-> Fixnum Byte Void))
        (define (dma-write addr value) (dma-write-macro addr value))
        (values clock reset get-state register-read register-write dma-write)))))

(define-ppu-compiler [#:compile-ppu compile-ppu-raw #:state-struct ppustate #:state-type PPUState]
  [scanline : Fixnum 0] ; int16
  [cycle : Fixnum -1] ; int16
  [ppumask : Fixnum 0] ; 8 bits of flags
  [ppuctrl : Fixnum 0] ; 8 bits of flags
  [ppustatus : Fixnum 0] ; 3 bits of flags (and 5 unused bits)
  [fine-x : Fixnum 0] ; uint8
  [ppu-data-buffer : Byte 0] ; uint8
  ;[frame-complete? : Boolean #f] ; boolean
  ;[nmi? : Boolean #f] ; boolean
  [clock-result : Fixnum 0]
  [address-latch? : Boolean #f] ; boolean
  [oam-addr : Byte 0]
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
  [tram-addr : Fixnum 0]
  ; Foreground
  [sprite-count : Fixnum 0]
  [sprite-zero-hit-possible? : Boolean #f]
  [sprite-zero-being-rendered? : Boolean #f]
  )

(define-syntax (compile-ppu stx)
  (syntax-case stx ()
    [(_ stuff ...)
     (with-syntax ([ooo (quote-syntax ...)])
       (syntax/loc stx
         (compile-ppu-raw #:ooo ooo stuff ...)))]))

(module+ test
  (define-syntax (wish stx)
    (syntax-case stx ()
      [(_ #:ppu-read addr ppumask)
       #'(ann 0 Byte)]
      [(_ #:ppu-write addr val)
       #'(error "TODO ppu-write")]
      [else (raise-syntax-error #f "unrecognized wish" stx)]))
  (define-values (clock reset get-state register-read register-write dma-write)
    (compile-ppu #:wish wish))
  (reset)
  ;(println (get-state))
  (clock)
  )
