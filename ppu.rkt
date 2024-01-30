#lang typed/racket

(require "ufx.rkt"
         "util.rkt"
         (for-syntax racket/fixnum))

(define-stxparams
  #:errmsg "PPU syntax not parameterized"
  scanline ; int16
  cycle ; int16
  ppumask ; 8 bits of flags
  ppuctrl ; 8 bits of flags
  ppustatus ; 3 bits of flags (and 5 unused bits)
  fine-x ; uint8
  frame-complete? ; boolean
  nmi? ; boolean

  ; Background rendering
  bg-next-tile-lsb ; uint8
  bg-next-tile-msb ; uint8
  bg-next-tile-id ; uint8
  bg-next-tile-attrib ; uint8
  bg-shifter-pattern-lo ; uint16
  bg-shifter-pattern-hi ; uint16
  bg-shifter-attrib-lo ; uint16
  bg-shifter-attrib-hi ; uint16

  ppu-read ; (-> Fixnum Byte)

  vram-addr ; loopy, uint16
  tram-addr ; loopy, uint16
  )

; Masks for "loopy registers"
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
  [#b00000100 /increment-mode?])

; Masks for PPUMASK
(define-maskers
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

(define-syntax-rule (shift-left! id ...)
  (begin (SET! id (ufxand #xFFFF (ufxlshift id 1)))
         ...))

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
    (when (and (ufx>= scanline -1)
               (ufx< scanline 240))
      (when (and (ufx= 0 scanline)
                 (ufx= 0 cycle))
        ; OLC: "Odd Frame" cycle skip
        (SET! cycle 1))
      (when (and (ufx= -1 scanline)
                 (ufx= -1 cycle))
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
        (SET! nmi? #t)))
    
    ; Compose the pixel!
    (let ([bg-pixel : Byte 0]
          [bg-palette : Byte 0])
      (when (has-any-flag? ppumask /render-bg?)
        (let* ([bit-mux (ufxrshift #x8000 fine-x)]
               [p0-pixel (ufxand bg-shifter-pattern-lo bit-mux)]
               [p1-pixel (ufxand bg-shifter-pattern-hi bit-mux)]
               [bg-pal0 (ufxand bg-shifter-attrib-lo bit-mux)]
               [bg-pal1 (ufxand bg-shifter-attrib-hi bit-mux)])
          (set! bg-pixel (ufxand #xFF (ufxior p0-pixel (ufxlshift p1-pixel 1))))
          (set! bg-palette (ufxand #xFF (ufxior bg-pal0 (ufxlshift bg-pal1 1))))))
      #;(println (list "TODO write pixel:" bg-pixel bg-palette scanline cycle)))

    ; Advance renderer
    (SET! cycle (ufx+ 1 cycle))
    (when (ufx>= cycle 341)
      (SET! cycle 0)
      (SET! scanline (ufx+ 1 scanline))
      (when (ufx>= scanline 261)
        (SET! scanline -1)
        (SET! frame-complete? #t)))))

(module+ test
  (require typed/rackunit
           racket/stxparam)
  (define randomizer : Fixnum (random 9999))
  (: .ppu-read (-> Fixnum Byte))
  (define (.ppu-read addr)
    ; Do just enough to prevent unrealistic optimizations and/or branch predictions
    (set! randomizer (ufx+ randomizer (ufxxor randomizer addr)))
    (ufxand 255 randomizer))
  (syntax-parameterize ([ppu-read (lambda (stx)
                                    (syntax-case stx ()
                                      [(_ arg ...) #'(.ppu-read arg ...)]))])
    (let ([.scanline : Fixnum 0]
          [.cycle : Fixnum 0] ; int16
          [.ppuctrl : Fixnum 0] ; uint8, we'll just ignore the excess bytes (hopefully!)
          [.ppumask : Fixnum 0] ; uint8, we'll just ignore the excess bytes (hopefully!)
          [.ppustatus : Fixnum 0] ; uint8, we'll just ignore the excess bytes (hopefully!)
          [.fine-x : Byte 0]
          [.frame-complete? : Boolean #f]
          [.nmi? : Boolean #f]
          [.bg-shifter-pattern-lo : Fixnum 0] ; uint16
          [.bg-shifter-pattern-hi : Fixnum 0] ; uint16
          [.bg-shifter-attrib-lo : Fixnum 0] ; uint16
          [.bg-shifter-attrib-hi : Fixnum 0] ; uint16
          [.bg-next-tile-lsb : Byte 0]
          [.bg-next-tile-msb : Byte 0]
          [.bg-next-tile-id : Byte 0]
          [.bg-next-tile-attrib : Fixnum 0] ; seems to be clamped down to just the 2 lowest bits??
          [.vram-addr : Fixnum 0]
          [.tram-addr : Fixnum 0]
          )
      (parameterize-syntax-ids
       ([scanline .scanline]
        [cycle .cycle]
        [ppuctrl .ppuctrl]
        [ppumask .ppumask]
        [ppustatus .ppustatus]
        [fine-x .fine-x]
        [frame-complete? .frame-complete?]
        [nmi? .nmi?]
        [bg-shifter-pattern-lo .bg-shifter-pattern-lo]
        [bg-shifter-pattern-hi .bg-shifter-pattern-hi]
        [bg-shifter-attrib-lo .bg-shifter-attrib-lo]
        [bg-shifter-attrib-hi .bg-shifter-attrib-hi]
        [bg-next-tile-lsb .bg-next-tile-lsb]
        [bg-next-tile-msb .bg-next-tile-msb]
        [bg-next-tile-id .bg-next-tile-id]
        [bg-next-tile-attrib .bg-next-tile-attrib]
        [vram-addr .vram-addr]
        [tram-addr .tram-addr])
       (let ()
         (define (do-clock) (clock))
         ; Each frame is 340 cycles * 260 scanlines.
         (println "Time for 600 frames:")
         (time (for ([i (in-range (* 340 260 600))])
                 (do-clock))))))))
