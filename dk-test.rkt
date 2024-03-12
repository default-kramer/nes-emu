#lang typed/racket

(provide bus-clock TODO bus-reset
         current-pixel
         RGB
         current-pixel-x
         current-pixel-y)

(require (prefix-in cpu: "cpu.rkt")
         (prefix-in ppu: "core/ppu.rkt")
         "ufx.rkt"
         "util.rkt"
         racket/stxparam
         racket/unsafe/ops)

(module+ test
  (require typed/rackunit))

(: cart-bytes Bytes)
(define cart-bytes
  (let* ([port (open-input-file "Donkey Kong (Japan).nes")]
         #;[port (open-input-file "Bomberman (USA).nes")]
         #;[port (open-input-file "Dig Dug (Japan).nes")] ; note: good 8x16 test case?
         #;[port (open-input-file "Mario Bros. (Europe) (Rev A).nes")]
         #;[port (open-input-file "Mario Bros. (World).nes")]
         #;[port (open-input-file "Excitebike (Japan, USA).nes")]
         #;[port (open-input-file "Galaga (Japan).nes")] ; another 8x16 test
         [bytes (read-bytes 16 port)] ; discard first 16 bytes (the header), I'll deal with this later
         [bytes (read-bytes #x6000 port)])
    (close-input-port port)
    (if (eof-object? bytes)
        (error ".nes file did not contain enough bytes")
        bytes)))

(define-syntax-rule (define-multi [a ...] ...)
  (begin (define a ...)
         ...))

(define-multi
  [system-clock : Fixnum 0]
  [frame-count : Fixnum 0]
  [bg-hash : Fixnum 17]
  [cpu-cycles : Fixnum 0]
  [frame-complete? : Boolean #f])

; PPU stuff that's actually hidden from the PPU implementation behind
; the read/write syntax parameters.
(define tbl-name (make-bytes #x800)) ; 2 tables of 1024 bytes each
(define tbl-pattern (make-bytes #x2000)) ; 2 tables of 4096 bytes each
(define tbl-palette (make-bytes 32)) ; one 32-byte table

(: nametable-index (-> Fixnum Fixnum))
(define (nametable-index addr)
  ; TODO assuming Horizontal mirror, meaning that
  ;  000-3FF => 000-3FF
  ;  400-7FF => 000-3FF
  ;  800-BFF => 400-7FF
  ;  C00-FFF => 400-7FF
  (let ([second-table? (ufxand #x800 addr)])
    (ufxior (ufxand #x3FF addr)
            (ufxrshift second-table? 1)))
  ; For reference, vertical mirror means that
  ;  000-3FF => 000-3FF
  ;  400-7FF => 400-7FF
  ;  800-BFF => 000-3FF
  ;  C00-FFF => 400-7FF
  )

(: palette-table-index (-> Fixnum Fixnum))
(define (palette-table-index addr)
  (let ([addr (ufxand #x1F addr)])
    (case addr
      [(#x10) #x0] ; what are these translations for??
      [(#x14) #x4]
      [(#x18) #x8]
      [(#x1C) #xC]
      [else addr])))

(: ppu-read (-> Fixnum Fixnum Byte))
(define (ppu-read addr ppumask)
  (let ([addr (ufxand #x3FFF addr)])
    (cond
      [(ufx< addr #x2000)
       (let ([result
              ; Mapper 0 specific! Read from vCHRMemory (cartridge $4000 - $5FFF)
              (unsafe-bytes-ref cart-bytes (ufx+ #x4000 (ufxand #x1FFF addr)))])
         #;(when (ufx= 439187 system-clock)
             (println (list "ppu-read" addr result ppuctrl bg-next-tile-id vram-addr)))
         result)]
      [(ufx< addr #x2000)
       ; I assume this is used when the mapper doesn't intercept this range?
       (unsafe-bytes-ref tbl-pattern (ufxand #x1FFF addr))]
      [(ufx< addr #x3F00)
       (let* ([index (nametable-index addr)]
              [result (unsafe-bytes-ref tbl-name index)])
         #;(when (ufx= 439119 system-clock)
             (println (list "ppu-read" addr result index)))
         result)]
      [(ufx< addr #x4000)
       (let* ([index (palette-table-index addr)]
              [raw (unsafe-bytes-ref tbl-palette index)]
              ; lowest bit is grayscale flag
              [mask (if (ufx= 0 (ufxand 1 ppumask))
                        #x3F
                        #x30)])
         (ufxand raw mask))]
      [else 0])))

; see olc2C02::ppuWrite
(: ppu-write (-> Fixnum Fixnum Void))
(define (ppu-write addr value)
  ; Donkey Kong does not respond to PPU writes
  (let ([addr (ufxand #x3FFF addr)])
    (cond
      [(ufx< addr #x2000)
       (unsafe-bytes-set! tbl-pattern (ufxand #x1FFF addr) value)]
      [(ufx< addr #x3F00)
       (let* ([addr (ufxand #xFFF addr)]
              [index (nametable-index addr)])
         #;(display* #:port ppulog
                     " tbl-name update "addr" "value"\n")
         (unsafe-bytes-set! tbl-name index value))]
      [(ufx< addr #x4000)
       (let ([index (palette-table-index addr)])
         (unsafe-bytes-set! tbl-palette index value))])))


(define-type RGB (Immutable-Vector Byte Byte Byte))

(define current-pixel : RGB #(0 0 0))
(define current-pixel-x : Fixnum 0)
(define current-pixel-y : Fixnum 0)
(define log-pixel-addr : Fixnum 0)
(define log-pixel-index : Fixnum 0)

(: pixel->number (-> RGB Fixnum))
(define (pixel->number current-pixel)
  (ufxior* #xFF000000 ; alpha
           (ufxlshift (unsafe-vector-ref current-pixel 0) 0) ; r
           (ufxlshift (unsafe-vector-ref current-pixel 1) 8)  ; g
           (ufxlshift (unsafe-vector-ref current-pixel 2) 16)  ; b
           ))

(define-syntax-rule (make-pal-screen [idx r g b] ...)
  #(#(r g b)
    ...))

(: pal-screen (Immutable-Vectorof RGB))
(define pal-screen (make-pal-screen [0x00 84 84 84]
                                    [0x01 0 30 116]
                                    [0x02 8 16 144]
                                    [0x03 48 0 136]
                                    [0x04 68 0 100]
                                    [0x05 92 0 48]
                                    [0x06 84 4 0]
                                    [0x07 60 24 0]
                                    [0x08 32 42 0]
                                    [0x09 8 58 0]
                                    [0x0A 0 64 0]
                                    [0x0B 0 60 0]
                                    [0x0C 0 50 60]
                                    [0x0D 0 0 0]
                                    [0x0E 0 0 0]
                                    [0x0F 0 0 0]

                                    [0x10 152 150 152]
                                    [0x11 8 76 196]
                                    [0x12 48 50 236]
                                    [0x13 92 30 228]
                                    [0x14 136 20 176]
                                    [0x15 160 20 100]
                                    [0x16 152 34 32]
                                    [0x17 120 60 0]
                                    [0x18 84 90 0]
                                    [0x19 40 114 0]
                                    [0x1A 8 124 0]
                                    [0x1B 0 118 40]
                                    [0x1C 0 102 120]
                                    [0x1D 0 0 0]
                                    [0x1E 0 0 0]
                                    [0x1F 0 0 0]

                                    [0x20 236 238 236]
                                    [0x21 76 154 236]
                                    [0x22 120 124 236]
                                    [0x23 176 98 236]
                                    [0x24 228 84 236]
                                    [0x25 236 88 180]
                                    [0x26 236 106 100]
                                    [0x27 212 136 32]
                                    [0x28 160 170 0]
                                    [0x29 116 196 0]
                                    [0x2A 76 208 32]
                                    [0x2B 56 204 108]
                                    [0x2C 56 180 204]
                                    [0x2D 60 60 60]
                                    [0x2E 0 0 0]
                                    [0x2F 0 0 0]

                                    [0x30 236 238 236]
                                    [0x31 168 204 236]
                                    [0x32 188 188 236]
                                    [0x33 212 178 236]
                                    [0x34 236 174 236]
                                    [0x35 236 174 212]
                                    [0x36 236 180 176]
                                    [0x37 228 196 144]
                                    [0x38 204 210 120]
                                    [0x39 180 222 120]
                                    [0x3A 168 226 144]
                                    [0x3B 152 226 180]
                                    [0x3C 160 214 228]
                                    [0x3D 160 162 160]
                                    [0x3E 0 0 0]
                                    [0x3F 0 0 0]))

(: ppu-reset (-> Void))
(: ppu-clock (-> (Values Fixnum Fixnum Fixnum)))
(: ppu-get-state (-> ppu:PPUState))
(: ppu-register-write (-> Fixnum Byte Void))
(: ppu-register-read (-> Fixnum Byte))
(: ppu-dma-write (-> Fixnum Byte Void))
(define-values (ppu-clock ppu-reset ppu-get-state ppu-register-read ppu-register-write ppu-dma-write)
  (let ()
    (define-syntax (wish stx)
      (syntax-case stx ()
        [(_ #:ppu-read addr ppumask)
         (syntax/loc stx (ppu-read addr ppumask))]
        [(_ #:ppu-write addr val)
         (syntax/loc stx (ppu-write addr val))]))
    (ppu:compile-ppu #:wish wish)))

; Have to define the CPU after the PPU because cpu-write
; needs to contain ppu-register-write.

(define cpu-ram (make-bytes #x800)) ; 2048 bytes

(define dma-page : Fixnum 0) ; shifted immediately, so we can just ior with dma-addr each time
(define dma-addr : Fixnum 0)
(define dma-byte : Byte 0)
(define dma-transfer? : Boolean #f)
(define dma-delay? : Boolean #f)

(define last-opcode-read : Fixnum -1) ; TEMP help log opcodes
;(define cpulog (open-output-file "my-cpulog.txt" #:exists 'replace))
(define-syntax-rule (display* #:port port item ...)
  (begin (display item port)
         ...))

(: cpu-read (-> Fixnum Byte))
(define (cpu-read addr)
  (define result
    (cond
      [(ufx< addr #x2000)
       ; OLC: System RAM Address Range, mirrored every 2048
       (let ([result (unsafe-bytes-ref cpu-ram (ufxand #x7FF addr))])
         #;(when (ufx= addr 1776)
             (println (list "1776 read" result system-clock)))
         result)]
      [(ufx< addr #x4000)
       (let ([result (ppu-register-read addr)])
         #;(when (ufx= 2 (ufxand 7 addr))
             (display* #:port ppulog
                       "read ppureg 2 produces "result"\n"))
         result)]
      ; TODO also need controller stuff at $4016
      [(and (ufx>= addr #x8000)
            (ufx<= addr #xFFFF))
       ; Mapper 0 specific! And assumes nPRGBanks is 1
       ; Read from vPRGMemory (cartridge $0000 - $3FFF)
       (unsafe-bytes-ref cart-bytes (ufxand #x3FFF addr))]
      [else 0]))
  #;(when (and (ufx= cpu-cycles 0)
               (ufx= addr PC)
               (not (ufx= system-clock last-opcode-read)))
      (set! last-opcode-read system-clock)
      #;(display* #:port cpulog
                  system-clock" "PC" "result" "A" "X" "Y" "SP"\n"))
  result)

; see Bus::cpuWrite
(: cpu-write (-> Fixnum Fixnum Void))
(define (cpu-write addr value)
  (cond
    [(ufx< addr #x2000)
     ; OLC: System RAM Address Range
     (unsafe-bytes-set! cpu-ram (ufxand #x7FF addr) value)]
    [(ufx< addr #x4000)
     (begin
       #;(when (ufx= 6 (ufxand 7 addr))
           (display* #:port ppulog
                     "cpuwrite case 6.? "value"\n"))
       #;(println (list "GOT CPU WRITE TO PPU" addr cycle scanline system-clock))
       (ppu-register-write addr (ufxand 255 value)))]
    [(ufx= addr #x4014)
     (begin ; start DMA
       (set! dma-page (ufxlshift (ufxand 255 value) 8))
       (set! dma-addr 0)
       (set! dma-transfer? #t)
       (set! dma-delay? #t))]
    ; TODO also need controller stuff at $4016
    [else
     (void)]))

(define cpu (cpu:make-cpu cpu-read cpu-write))

(: cpu-reset (-> Fixnum))
(define (cpu-reset) (cpu:cpu-reset cpu))
(: cpu-step (-> Fixnum))
(define (cpu-step) (cpu:cpu-step cpu))
(: cpu-nmi (-> Fixnum))
(define (cpu-nmi) (cpu:cpu-nmi cpu))

(: print-some-bytes (-> Fixnum (Listof Byte)))
(define (print-some-bytes addr)
  (for/list ([i (in-range 10)])
    (bytes-ref cart-bytes (ufx+ addr i))))


;;; So I think the PPU and CPU are complete!?!
;;; And now we just have to do the bus stuff...

(define-syntax-rule (adjust-cycles expr)
  ; Let's immediately adjust the CPU cycles to match the system clock
  #;(ufx* 3 expr)
  ; NOPE can't do that... because the NMI is not guaranteed to happen on
  ; a multiple of 3, so we need to do it the other way
  expr)

(define (bus-reset)
  (set! cpu-cycles (adjust-cycles (cpu-reset)))
  (ppu-reset)
  (set! system-clock 0)
  (set! dma-page 0)
  (set! dma-addr 0)
  (set! dma-byte 0)
  (set! dma-delay? #t)
  (set! dma-transfer? #f))

(define-syntax-rule (compose-pixel2 x y index)
  (let ([x (ann x Fixnum)]
        [y (ann y Fixnum)]
        [index (ann index Fixnum)])
    #;(assert 0 <= index <= #x3F)
    ;(define hash-input (ufxior bg-pixel (ufxlshift bg-palette 2)))
    (set! bg-hash (ufxand #x3FFFFFFF (ufx+ index (ufx* bg-hash 31))))
    ;(set! log-pixel-addr addr)
    (set! log-pixel-index index)
    (set! current-pixel (unsafe-vector-ref pal-screen index))
    (set! current-pixel-x x)
    (set! current-pixel-y y)))

(define (bus-clock [ppulog : (U #f Output-Port) #f])
  (define-syntax-rule (has-bit? bit val)
    (ufx= bit (ufxand bit val)))
  (define-values (ppu-clock-result pixel-x pixel-y)
    (ppu-clock))

  ; WARNING should be using constants for these masks:
  (set! frame-complete? (has-bit? #x80 ppu-clock-result))
  (define nmi? (has-bit? #x40 ppu-clock-result))
  (define pixel-index (ufxand #x3F ppu-clock-result))
  (compose-pixel2 pixel-x pixel-y pixel-index)

  ; Log PPU stuff before stepping the CPU to match my hacked up OLC logging
  #;(when (and (frame-count . > . 1355)
               (frame-count . < . 1358))
      (display* #:port ppulog
                system-clock" "current-pixel-x","current-pixel-y
                "="log-pixel-index" "log-pixel-addr" "vram-addr" "tram-addr
                " "ppustatus" "ppu-data-buffer" "bg-shifter-pattern-lo" "fine-x
                " "bg-next-tile-msb" "bg-next-tile-lsb" "bg-next-tile-id"\n"))
  #;(when ppulog
      (display* #:port ppulog pixel-x","pixel-y" = "pixel-index"\n"))
  (when frame-complete?
    (set! frame-count (ufx+ 1 frame-count))
    (when ppulog
      (display* #:port ppulog
                frame-count" "bg-hash"\n")))

  (when (ufx= 0 (ufxmodulo system-clock 3))
    (if dma-transfer?
        (if dma-delay?
            (when (ufx= 1 (ufxmodulo system-clock 2))
              (set! dma-delay? #f))
            (if (ufx= 0 (ufxmodulo system-clock 2))
                ; on even clock cycles, read a byte
                (set! dma-byte (cpu-read (ufxior dma-page dma-addr)))
                (begin ; on odd clock cycles, write the byte PPU
                  (ppu-dma-write dma-addr dma-byte)
                  (set! dma-addr (ufx+ 1 dma-addr))
                  (when (ufx= dma-addr 256)
                    (set! dma-transfer? #f)
                    (set! dma-delay? #t)))))
        (begin ; no DMA
          (when (ufx= 0 cpu-cycles)
            (set! cpu-cycles (adjust-cycles (cpu-step))))
          (set! cpu-cycles (ufx- cpu-cycles 1)))))

  (when nmi?
    (set! cpu-cycles (adjust-cycles (cpu-nmi))))

  (set! system-clock (ufx+ 1 system-clock)))

(: TODO (-> Boolean))
(define (TODO)
  (and frame-complete?
       (begin (set! frame-complete? #f)
              #t)))

(module+ test
  (let ()
    (define actual-path "./test-logs/dk-test.actual.bghash.txt")
    (define expected-path "./test-logs/dk-test.expected.bghash.txt")
    (bus-reset)
    (let ([ppulog (open-output-file actual-path #:exists 'replace)])
      (with-handlers ([exn? (lambda (e)
                              (println (list "FATAL ERROR" e)))])
        (let loop ()
          (bus-clock ppulog)
          (when (< frame-count 2370)
            (loop))))
      (close-output-port ppulog))
    (let* ([expected (port->string (open-input-file expected-path))]
           [actual (port->string (open-input-file actual-path))])
      (when (not (equal? actual expected))
        (fail (format "files differ: ~a vs ~a" expected-path actual-path))))))
