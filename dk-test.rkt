#lang racket
(define-syntax-rule (: stuff ...)
  (begin))

(provide bus-clock TODO bus-reset
         current-pixel
         ;RGB
         current-pixel-x
         current-pixel-y)

(require (prefix-in cpu: "cpu.rkt")
         (prefix-in ppu: "ppu.rkt")
         "ufx.rkt"
         "util.rkt"
         racket/stxparam
         racket/unsafe/ops)

(: cart-bytes Bytes)
(define cart-bytes
  (let* ([port (open-input-file "Donkey Kong (Japan).nes")]
         [bytes (read-bytes 16 port)] ; discard first 16 bytes (the header), I'll deal with this later
         [bytes (read-bytes #x6000 port)])
    (close-input-port port)
    (if (eof-object? bytes)
        (error ".nes file did not contain enough bytes")
        bytes)))

(define-syntax-rule (define-multi [a : type val] ...)
  (begin (define a val)
         ...))

(define-multi
  ; CPU registers
  [PC : Fixnum 0]
  [A : Fixnum 0]
  [X : Fixnum 0]
  [Y : Fixnum 0]
  [SP : Fixnum 0]
  [P : Fixnum 0]
  ; PPU registers
  [scanline : Fixnum 0]
  [cycle : Fixnum 0]
  [ppuctrl : Fixnum 0]
  [ppumask : Fixnum 0]
  [ppustatus : Fixnum 0]
  [fine-x : Fixnum 0]
  [ppu-data-buffer : Byte 0]
  [frame-complete? : Boolean #f]
  [nmi? : Boolean #f]
  [address-latch? : Boolean #f]
  [bg-shifter-pattern-lo : Fixnum 0]
  [bg-shifter-pattern-hi : Fixnum 0]
  [bg-shifter-attrib-lo : Fixnum 0]
  [bg-shifter-attrib-hi : Fixnum 0]
  [bg-next-tile-lsb : Fixnum 0]
  [bg-next-tile-msb : Fixnum 0]
  [bg-next-tile-id : Fixnum 0]
  [bg-next-tile-attrib : Fixnum 0]
  [vram-addr : Fixnum 0]
  [tram-addr : Fixnum 0]
  )

(define-multi
  [system-clock : Fixnum 0]
  [cpu-cycles : Fixnum 0])

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

(: ppu-read (-> Fixnum Byte))
(define (ppu-read addr)
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


;(define-type RGB (Immutable-Vector Byte Byte Byte))

(define current-pixel  #(0 0 0))
(define current-pixel-x 0)
(define current-pixel-y 0)
(define log-pixel-addr  0)
(define log-pixel-index 0)

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

(: compose-pixel! (-> Fixnum Fixnum Byte Byte Any))
(define (compose-pixel! cycle scanline bg-palette bg-pixel)
  (let* ([x (ufx- cycle 1)]
         [y scanline]
         [addr (ufxior* #x3F00
                        (ufxlshift bg-palette 2)
                        bg-pixel)]
         [index (ppu-read addr)])
    #;(when (or (ufx= system-clock 357192)
                (ufx= system-clock 357191)
                (ufx= system-clock 357190))
        (let* ([bit-mux (ufxrshift #x8000 fine-x)]
               [p0-pixel (ufxand bg-shifter-pattern-lo bit-mux)]
               [p1-pixel (ufxand bg-shifter-pattern-hi bit-mux)]
               [bg-pal0 (ufxand bg-shifter-attrib-lo bit-mux)]
               [bg-pal1 (ufxand bg-shifter-attrib-hi bit-mux)]
               [bg-pixel (ufxand #xFF (ufxior p0-pixel (ufxlshift p1-pixel 1)))]
               [bg-palette (ufxand #xFF (ufxior bg-pal0 (ufxlshift bg-pal1 1)))])
          (println (list 'ppumask ppumask
                         'cycle cycle
                         'scanline scanline
                         'bg-shifter-pattern-lo bg-shifter-pattern-lo
                         'bg-shifter-pattern-hi bg-shifter-pattern-hi
                         'bit-mux bit-mux
                         'p0-pixel p0-pixel
                         'p1-pixel p1-pixel
                         'bg-pal0 bg-pal0
                         'bg-pal1 bg-pal1
                         'bg-pixel bg-pixel
                         'bg-palette bg-palette))))
    (set! log-pixel-addr addr)
    (set! log-pixel-index index)
    (set! current-pixel (unsafe-vector-ref pal-screen (ufxand #x3F index)))
    (set! current-pixel-x x)
    (set! current-pixel-y y)))


(: ppu-reset (-> Void))
(: ppu-clock (-> Void))
(: ppu-register-write (-> Fixnum Fixnum Void))
(: ppu-register-read (-> Fixnum Byte))
(define-values (ppu-reset ppu-clock ppu-register-write ppu-register-read)
  (syntax-parameterize
      ([ppu:ppu-read (lambda (stx)
                       (syntax-case stx ()
                         [(_ arg ...) #'(ppu-read arg ...)]))]
       [ppu:ppu-write (lambda (stx)
                        (syntax-case stx ()
                          [(_ arg ...) #'(ppu-write arg ...)]))]
       [ppu:compose-pixel! (lambda (stx)
                             (syntax-case stx ()
                               [(_ arg ...) #'(compose-pixel! arg ...)]))])
    (parameterize-syntax-ids
     (  [ppu:scanline scanline]
        [ppu:cycle cycle]
        [ppu:ppuctrl ppuctrl]
        [ppu:ppumask ppumask]
        [ppu:ppustatus ppustatus]
        [ppu:fine-x fine-x]
        [ppu:ppu-data-buffer ppu-data-buffer]
        [ppu:frame-complete? frame-complete?]
        [ppu:nmi? nmi?]
        [ppu:address-latch? address-latch?]
        [ppu:bg-shifter-pattern-lo bg-shifter-pattern-lo]
        [ppu:bg-shifter-pattern-hi bg-shifter-pattern-hi]
        [ppu:bg-shifter-attrib-lo bg-shifter-attrib-lo]
        [ppu:bg-shifter-attrib-hi bg-shifter-attrib-hi]
        [ppu:bg-next-tile-lsb bg-next-tile-lsb]
        [ppu:bg-next-tile-msb bg-next-tile-msb]
        [ppu:bg-next-tile-id bg-next-tile-id]
        [ppu:bg-next-tile-attrib bg-next-tile-attrib]
        [ppu:vram-addr vram-addr]
        [ppu:tram-addr tram-addr])
     (define (ppu-reset) (ppu:reset))
     (define (ppu-clock) (ppu:clock))
     (define (ppu-reg-write addr value); [addr : Fixnum] [value : Fixnum])
       (ppu:cpu-write addr value))
     (define (ppu-reg-read addr); [addr : Fixnum])
       (ppu:cpu-read addr))
     (values ppu-reset ppu-clock ppu-reg-write ppu-reg-read))))

; Have to define the CPU after the PPU because cpu-write
; needs to contain ppu-register-write.

(define cpu-ram (make-bytes #x800)) ; 2048 bytes

(define last-opcode-read  -1) ; TEMP help log opcodes
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
       (ppu-register-write addr value))]
    ; TODO also need controller stuff at $4016
    [else
     (void)]))

(: cpu-reset (-> Fixnum))
(: cpu-step (-> Fixnum))
(: cpu-nmi (-> Fixnum))
(define-values (cpu-reset cpu-step cpu-nmi)
  (syntax-parameterize
      ([cpu:cpu-read (lambda (stx)
                       (syntax-case stx ()
                         [(_ arg ...) #'(cpu-read arg ...)]))]
       [cpu:cpu-write (lambda (stx)
                        (syntax-case stx ()
                          [(_ arg ...) #'(cpu-write arg ...)]))])
    (parameterize-syntax-ids
     (  [cpu:PC PC]
        [cpu:A A]
        [cpu:X X]
        [cpu:Y Y]
        [cpu:SP SP]
        [cpu:P P])
     (define (cpu-reset) (cpu:reset))
     (define (cpu-step) (cpu:step))
     (define (cpu-nmi) (cpu:nmi))
     (values cpu-reset cpu-step cpu-nmi))))

(: print-some-bytes (-> Fixnum (Listof Byte)))
(define (print-some-bytes addr)
  (for/list ([i (in-range 10)])
    (bytes-ref cart-bytes (ufx+ addr i))))


;;; So I think the PPU and CPU are complete!?!
;;; And now we just have to do the bus stuff...

(define-syntax-rule (adjust-cycles expr)
  ; Let's immediately adjust the CPU cycles to match the system clock
  (ufx* 3 expr))

(define (bus-reset)
  (set! cpu-cycles (adjust-cycles (cpu-reset)))
  (ppu-reset)
  (set! system-clock 0))

;(define ppulog (open-output-file "my-ppulog.txt" #:exists 'replace))

(define (blah)
  (adjust-cycles (cpu-step)))

(define (todo-cpu)
  (when (ufx= 0 cpu-cycles)
    (set! cpu-cycles (blah)))
  (set! cpu-cycles (ufx- cpu-cycles 1)))

(define (todo-nmi)
  (when nmi?
    (set! nmi? #f)
    (set! cpu-cycles (adjust-cycles (cpu-nmi)))))

(define (bus-clock)
  (ppu-clock)
  ; Log PPU stuff before stepping the CPU to match my hacked up OLC logging
  #;(display* #:port ppulog
              system-clock" "current-pixel-x","current-pixel-y
              "="log-pixel-index" "log-pixel-addr" "vram-addr
              " "ppustatus" "ppu-data-buffer" "bg-shifter-pattern-lo" "fine-x
              " "bg-next-tile-msb" "bg-next-tile-lsb" "bg-next-tile-id"\n")

  ;(when (ufx= 0 (ufxmodulo system-clock 3))
  ;(when (ufx= 0 cpu-cycles)
  ; (set! cpu-cycles (adjust-cycles (cpu-step))))
  ;(set! cpu-cycles (ufx- cpu-cycles 1))
  (todo-cpu)
  ;(when nmi?
  ; (set! nmi? #f)
  ;(set! cpu-cycles (adjust-cycles (cpu-nmi))))
  (todo-nmi)
  (set! system-clock (ufx+ 1 system-clock)))

(: TODO (-> Boolean))
(define (TODO)
  (and frame-complete?
       (begin (set! frame-complete? #f)
              #t)))


(module+ test
  (bus-reset)
  (with-handlers ([exn? (lambda (e)
                          (println (list "FATAL ERROR" e)))])
    (for ([i (in-range 714387 #;982411)])
      (bus-clock)))
  #;(close-output-port cpulog)
  #;(close-output-port ppulog))
