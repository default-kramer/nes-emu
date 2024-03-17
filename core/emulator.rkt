#lang typed/racket

(provide make-emulator Pixel-Callback
         (struct-out emulator)
         (struct-out rom-info))

(require "../ufx.rkt"
         (prefix-in cpu: "cpu.rkt")
         (prefix-in ppu: "ppu.rkt")
         racket/unsafe/ops)

(struct rom-info ([prg-rom-size : Fixnum]
                  [chr-rom-size : Fixnum]
                  [mapper : Byte]
                  ; AMBIGUITY WARNING - Improve terminology?
                  ; 'horizontal means "vertical arrangement ("horizontal mirrored") (CIRAM A10 = PPU A11)"
                  ; 'vertical means "horizontal arrangement ("vertically mirrored") (CIRAM A10 = PPU A10)"
                  [mirror : (U 'vertical 'horizontal)])
  #:transparent
  #:type-name ROM-Info)

(struct emulator ([rom-info : ROM-Info]
                  [reset : (-> Void)]
                  ; clock ::: TODO returns true on frame-complete.
                  ; Possibly should be doing something more well-defined...
                  [clock : (-> Boolean)])
  #:transparent
  #:type-name Emulator)

(: get-rom-info (-> Bytes (U ROM-Info String)))
(define (get-rom-info header)
  (define length (bytes-length header))
  (define (check-signature)
    (and (= #x4E (bytes-ref header 0))
         (= #x45 (bytes-ref header 1))
         (= #x53 (bytes-ref header 2))
         (= #x1A (bytes-ref header 3))))
  (cond
    [(not (= length 16))
     "Header must be 16 bytes"]
    [(not (check-signature))
     "iNES header is missing"]
    [else
     (let* ([prg-rom-size (ufx* #x4000 (bytes-ref header 4))]
            [chr-rom-size (ufx* #x2000 (bytes-ref header 5))]
            [byte6 (bytes-ref header 6)]
            [byte7 (bytes-ref header 7)]
            [mapper (ufxior (ufxrshift (ufxand #xF0 byte7) 0)
                            (ufxrshift (ufxand #xF0 byte6) 4))]
            [mapper (ufxand 255 mapper)]
            [mirror (if (ufx= 0 (ufxand 1 byte6))
                        'horizontal ; vertical arrangement ("horizontal mirrored") (CIRAM A10 = PPU A11)
                        'vertical ; horizontal arrangement ("vertically mirrored") (CIRAM A10 = PPU A10)
                        )])
       (rom-info prg-rom-size chr-rom-size mapper mirror))]))

(define zero-bytes (make-bytes 0))

; x y index -> Any
(define-type Pixel-Callback (-> Fixnum Fixnum Fixnum Any))

(: make-emulator (-> Path-String Pixel-Callback Emulator))
(define (make-emulator rom-path pixel-callback)

  (: cart-bytes Bytes)
  (define-values (rom-info-or-error cart-bytes)
    (let ()
      (define (go [port : Input-Port])
        (let ([header (read-bytes 16 port)])
          (if (eof-object? header)
              (values "File lacks iNES header (too small)" zero-bytes)
              (let ([rom-info (get-rom-info header)])
                (if (string? rom-info)
                    (values rom-info zero-bytes)
                    (values rom-info (port->bytes port)))))))
      (let*-values ([(port) (open-input-file rom-path)]
                    [(a b) (go port)])
        (close-input-port port)
        (values a b))))

  (: rom-info ROM-Info)
  (define rom-info (if (string? rom-info-or-error)
                       (error (format "~a (~a)" rom-info-or-error rom-path))
                       rom-info-or-error))

  ; TODO not sure about mapper 3... The same mapper0 code seems to
  ; work for Galaga but maybe that ROM's header is lying?
  (when (not (member (rom-info-mapper rom-info) '(0 3)))
    (error "Unsupported mapper:" (rom-info-mapper rom-info)))

  (define-syntax-rule (define-multi [a ...] ...)
    (begin (define a ...)
           ...))

  (define-multi
    [system-clock : Fixnum 0]
    [frame-count : Fixnum 0]
    [bg-hash : Fixnum 17]
    [cpu-cycles : Fixnum 0])

  ; PPU stuff that's actually hidden from the PPU implementation behind
  ; the read/write syntax parameters.
  (define tbl-name (make-bytes #x800)) ; 2 tables of 1024 bytes each
  (define tbl-pattern (make-bytes #x2000)) ; 2 tables of 4096 bytes each
  (define tbl-palette (make-bytes 32)) ; one 32-byte table

  (: nametable-index (-> Fixnum Fixnum))
  (define nametable-index
    (case (rom-info-mirror rom-info)
      [(horizontal)
       (lambda ([addr : Fixnum])
         ; 000-3FF => 000-3FF
         ; 400-7FF => 000-3FF
         ; 800-BFF => 400-7FF
         ; C00-FFF => 400-7FF
         (let ([second-table? (ufxand #x800 addr)])
           (ufxior (ufxand #x3FF addr)
                   (ufxrshift second-table? 1))))]
      [(vertical)
       (lambda ([addr : Fixnum])
         ; 000-3FF => 000-3FF
         ; 400-7FF => 400-7FF
         ; 800-BFF => 000-3FF
         ; C00-FFF => 400-7FF
         (ufxand #x7FF addr))]))

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
    (define result
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
    result)

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

  (define-syntax-rule (display* #:port port item ...)
    (begin (display item port)
           ...))

  (: cpu-read (-> Fixnum Byte))
  (define (cpu-read addr)
    (cond
      [(ufx< addr #x2000)
       ; OLC: System RAM Address Range, mirrored every 2048
       (let ([result (unsafe-bytes-ref cpu-ram (ufxand #x7FF addr))])
         result)]
      [(ufx< addr #x4000)
       (let ([result (ppu-register-read addr)])
         result)]
      ; TODO also need controller stuff at $4016
      [(and (ufx>= addr #x8000)
            (ufx<= addr #xFFFF))
       ; Mapper 0 specific! And assumes nPRGBanks is 1
       ; Read from vPRGMemory (cartridge $0000 - $3FFF)
       (unsafe-bytes-ref cart-bytes (ufxand #x3FFF addr))]
      [else 0]))

  ; see Bus::cpuWrite
  (: cpu-write (-> Fixnum Fixnum Void))
  (define (cpu-write addr value)
    (cond
      [(ufx< addr #x2000)
       ; OLC: System RAM Address Range
       (begin
         (unsafe-bytes-set! cpu-ram (ufxand #x7FF addr) value))]
      [(ufx< addr #x4000)
       (begin
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

  (: bus-clock (-> Boolean)) ; returns true when a frame completes
  (define (bus-clock)
    (define-syntax-rule (has-bit? bit val)
      (ufx= bit (ufxand bit val)))
    (define-values (ppu-clock-result pixel-x pixel-y)
      (ppu-clock))

    ; WARNING should be using constants for these masks:
    (define frame-complete? (has-bit? #x80 ppu-clock-result))
    (define nmi? (has-bit? #x40 ppu-clock-result))
    (define pixel-index (ufxand #x3F ppu-clock-result))
    (when frame-complete?
      (set! frame-count (ufx+ 1 frame-count)))
    #;(when (and ppulog frame-complete?)
        (display* #:port ppulog
                  frame-count" "bg-hash"\n"))
    (pixel-callback pixel-x pixel-y pixel-index)

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

    (set! system-clock (ufx+ 1 system-clock))
    frame-complete?)

  (emulator rom-info bus-reset bus-clock))
