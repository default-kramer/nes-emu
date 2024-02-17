#lang typed/racket

(provide PC A X Y SP P
         cpu-read cpu-write
         reset step nmi)

(require "nestest.rkt"
         "doodling.rkt"
         "util.rkt"
         "ufx.rkt"
         (submod "cpu-instructions.rkt" as-macros)
         racket/require
         (filtered-in
          (λ (name) ; Whenever I say `fx+` I actually mean `unsafe-fx+`
            (and (regexp-match #rx"^ufx" name)
                 (regexp-replace #rx"ufx" name "fx")))
          "ufx.rkt")
         racket/stxparam)

(module+ test
  (require typed/rackunit))

; The result of an addressing mode must be two values:
; 1. An address (could be absolute or relative)
; 2. The ExtraCyclesMask
(define-syntax-rule (define-mode (id) body ...)
  (define-syntax-rule (id)
    (ann (begin body ...)
         (Values Fixnum Fixnum))))

(define ignore-mode-cycle 0)
(define respect-mode-cycle 1)

(define-syntax-rule (define-registers id ...)
  (begin
    (define-syntax-parameter id
      (lambda (stx)
        (raise-syntax-error 'id "CPU register not parameterized")))
    ...))

(define-syntax-rule (define-flags [id bit] ...)
  (begin
    (define-syntax (id stx)
      (syntax-case stx ()
        [(_ #:mask)
         #'(ann bit Fixnum)]
        ; Flags are always used in an 8-bit context, so leave all higher bits off
        [(_ #:antimask)
         #`(ann #,(bitwise-xor #xFF (syntax-e #'bit)) Fixnum)]))
    ...))

(module+ test
  (check-equal? (Flag.B #:mask) 16)
  (check-equal? (Flag.B #:antimask) 239))

(define-flags [Flag.C 1]
  [Flag.Z 2]
  [Flag.I 4]
  [Flag.D 8]
  [Flag.B 16]
  [Flag.Unused 32]
  [Flag.V 64]
  [Flag.N 128])



; Convert number to hexadecimal string to match known good log
(define (~h [val : Fixnum] [width : Exact-Nonnegative-Integer 2])
  (~a (string-upcase (format "~x" val))
      #:width width #:align 'right #:left-pad-string "0"))

; MAIN SKELETON HERE!
(define-syntax-rule (def-emulator [#:emulate-one emu-one
                                   #:get-bytes get-instruction-bytes]
                      [handler opcode mode byte-count base-cycles unofficial-opcodes ...] ...)
  (begin
    (define-syntax-rule (emu-one op)
      ; The opcode is passed in
      (case op
        [(opcode unofficial-opcodes ...)
         (let-values ([([address : Fixnum] [extra-cycles-mask : Fixnum])
                       (mode)])
           ; Now we invoke the instruction handler, which is expected to do the work
           ; and return the total number of cycles
           (ann (handler #:address address #:base-cycles base-cycles #:extra-cycles-mask extra-cycles-mask #:opcode opcode)
                Fixnum))]
        ...
        [else (error (format "Invalid opcode: $~x [PC:~x A:~x X:~x Y:~x SP:~x P:~x"
                             op (ufx- PC 1) A X Y SP P))]))

    ; Helper for diagnostic usage:
    (: get-instruction-bytes (-> Fixnum (-> Fixnum Fixnum) (Listof Fixnum)))
    (define (get-instruction-bytes [pc : Fixnum] [cpu-read : (-> Fixnum Fixnum)])
      (let ([op (cpu-read pc)])
        (case op
          [(opcode unofficial-opcodes ...)
           (take (list (cpu-read pc) (cpu-read (fx+ 1 pc)) (cpu-read (fx+ 2 pc)))
                 byte-count)]
          ...
          [else (error (format "Invalid opcode: $~x" op))])))
    ))

(def-emulator [#:emulate-one emulate-one-instruction
               #:get-bytes get-instruction-bytes]
  ; == Branches ==
  ; These are the only instructions that use RELative addressing mode
  [BPL #x10 REL 2 2] ; negative clear ("positive")
  [BMI #x30 REL 2 2] ; negative set ("minus")
  [BVC #x50 REL 2 2] ; overflow clear
  [BVS #x70 REL 2 2] ; overflow set
  [BCC #x90 REL 2 2] ; carry clear
  [BCS #xB0 REL 2 2] ; carry set
  [BNE #xD0 REL 2 2] ; zero clear ("not equal")
  [BEQ #xF0 REL 2 2] ; zero set ("equal")
  ; == End Branches ==
  [ADC #x69 IMM 2 2]
  [ADC #x65 ZP0 2 3]
  [ADC #x75 ZPX 2 4]
  [ADC #x6D ABS 3 4]
  [ADC #x7D ABX 3 4]
  [ADC #x79 ABY 3 4]
  [ADC #x61 IZX 2 6]
  [ADC #x71 IZY 2 5]
  [AND #x29 IMM 2 2]
  [AND #x25 ZP0 2 3]
  [AND #x35 ZPX 2 4]
  [AND #x2D ABS 3 4]
  [AND #x3D ABX 3 4]
  [AND #x39 ABY 3 4]
  [AND #x21 IZX 2 6]
  [AND #x31 IZY 2 5]
  [BIT #x24 ZP0 2 3]
  [BIT #x2C ABS 3 4]
  [BRK #x00 IMP 1 7]
  [CLC #x18 IMP 1 2]
  [CLD #xD8 IMP 1 2]
  [CLI #x58 IMP 1 2]
  [CLV #xB8 IMP 1 2]
  [CMP #xC9 IMM 2 2]
  [CMP #xC5 ZP0 2 3]
  [CMP #xD5 ZPX 2 4]
  [CMP #xCD ABS 3 4]
  [CMP #xDD ABX 3 4]
  [CMP #xD9 ABY 3 4]
  [CMP #xC1 IZX 2 6]
  [CMP #xD1 IZY 2 5]
  [CPX #xE0 IMM 2 2]
  [CPX #xE4 ZP0 2 3]
  [CPX #xEC ABS 3 4]
  [CPY #xC0 IMM 2 2]
  [CPY #xC4 ZP0 2 3]
  [CPY #xCC ABS 3 4]
  [DEC #xC6 ZP0 2 5]
  [DEC #xD6 ZPX 2 6]
  [DEC #xCE ABS 3 6]
  [DEC #xDE ABX 3 7]
  [DEX #xCA IMP 1 2]
  [DEY #x88 IMP 1 2]
  [EOR #x49 IMM 2 2]
  [EOR #x45 ZP0 2 3]
  [EOR #x55 ZPX 2 4]
  [EOR #x4D ABS 3 4]
  [EOR #x5D ABX 3 4]
  [EOR #x59 ABY 3 4]
  [EOR #x41 IZX 2 6]
  [EOR #x51 IZY 2 5]
  [INC #xE6 ZP0 2 5]
  [INC #xF6 ZPX 2 6]
  [INC #xEE ABS 3 6]
  [INC #xFE ABX 3 7]
  [INX #xE8 IMP 1 2]
  [INY #xC8 IMP 1 2]
  [JMP #x4C ABS 3 3]
  [JMP #x6C IND 3 5]
  [JSR #x20 ABS 3 6]
  [LDA #xA9 IMM 2 2]
  [LDA #xA5 ZP0 2 3]
  [LDA #xB5 ZPX 2 4]
  [LDA #xAD ABS 3 4]
  [LDA #xBD ABX 3 4]
  [LDA #xB9 ABY 3 4]
  [LDA #xA1 IZX 2 6]
  [LDA #xB1 IZY 2 5]
  [LDX #xA2 IMM 2 2]
  [LDX #xA6 ZP0 2 3]
  [LDX #xB6 ZPY 2 4]
  [LDX #xAE ABS 3 4]
  [LDX #xBE ABY 3 4]
  [LDY #xA0 IMM 2 2]
  [LDY #xA4 ZP0 2 3]
  [LDY #xB4 ZPX 2 4]
  [LDY #xAC ABS 3 4]
  [LDY #xBC ABX 3 4]
  ; SHIFTS TODO
  [LSR #x4A IMP 1 2] ; actually "Accumulator"
  [LSR #x46 ZP0 2 5]
  [LSR #x56 ZPX 2 6]
  [LSR #x4E ABS 3 6]
  [LSR #x5E ABX 3 7]
  [ASL #x0A IMP 1 2] ; actually "Accumulator"
  [ASL #x06 ZP0 2 5]
  [ASL #x16 ZPX 2 6]
  [ASL #x0E ABS 3 6]
  [ASL #x1E ABX 3 7]
  [ROR #x6A IMP 1 2] ; actually "Accumulator"
  [ROR #x66 ZP0 2 5]
  [ROR #x76 ZPX 2 6]
  [ROR #x6E ABS 3 6]
  [ROR #x7E ABX 3 7]
  [ROL #x2A IMP 1 2] ; actually "Accumulator"
  [ROL #x26 ZP0 2 5]
  [ROL #x36 ZPX 2 6]
  [ROL #x2E ABS 3 6]
  [ROL #x3E ABX 3 7]
  ; END SHIFTS
  [NOP #xEA IMP 1 2 #x1A #x3A #x5A #x7A #xDA #xFA]
  [ORA #x09 IMM 2 2]
  [ORA #x05 ZP0 2 3]
  [ORA #x15 ZPX 2 4]
  [ORA #x0D ABS 3 4]
  [ORA #x1D ABX 3 4]
  [ORA #x19 ABY 3 4]
  [ORA #x01 IZX 2 6]
  [ORA #x11 IZY 2 5]
  [PHA #x48 IMP 1 3]
  [PHP #x08 IMP 1 3]
  [PLA #x68 IMP 1 4]
  [PLP #x28 IMP 1 4]
  [RTI #x40 IMP 1 6]
  [RTS #x60 IMP 1 6]
  [SBC #xE9 IMM 2 2 #xEB]
  [SBC #xE5 ZP0 2 3]
  [SBC #xF5 ZPX 2 4]
  [SBC #xED ABS 3 4]
  [SBC #xFD ABX 3 4]
  [SBC #xF9 ABY 3 4]
  [SBC #xE1 IZX 2 6]
  [SBC #xF1 IZY 2 5]
  [SEC #x38 IMP 1 2]
  [SED #xF8 IMP 1 2]
  [SEI #x78 IMP 1 2]
  [STA #x85 ZP0 2 3]
  [STA #x95 ZPX 2 4]
  [STA #x8D ABS 3 4]
  [STA #x9D ABX 3 5]
  [STA #x99 ABY 3 5]
  [STA #x81 IZX 2 6]
  [STA #x91 IZY 2 6]
  [STX #x86 ZP0 2 3]
  [STX #x96 ZPY 2 4]
  [STX #x8E ABS 3 4]
  [STY #x84 ZP0 2 3]
  [STY #x94 ZPX 2 4]
  [STY #x8C ABS 3 4]
  [TAX #xAA IMP 1 2]
  [TAY #xA8 IMP 1 2]
  [TXA #x8A IMP 1 2]
  [TYA #x98 IMP 1 2]
  [TXS #x9A IMP 1 2]
  [TSX #xBA IMP 1 2]
  ; == Begin unofficial ==
  [DCP #f ZP0 2 5 #xC7]
  [DCP #f ZPX 2 6 #xD7]
  [DCP #f IZX 2 8 #xC3]
  [DCP #f IZY 2 7 #xD3]
  [DCP #f ABS 3 6 #xCF]
  [DCP #f ABX 3 6 #xDF]
  [DCP #f ABY 3 6 #xDB]
  ; The unofficial ISB is called "ISC" elsewhere, but I'll use ISB to easily match the good log.
  ; It seems the timing matches DCP...
  [ISB #f ZP0 2 5 #xE7]
  [ISB #f ZPX 2 6 #xF7]
  [ISB #f IZX 2 8 #xE3]
  [ISB #f IZY 2 7 #xF3]
  [ISB #f ABS 3 6 #xEF]
  [ISB #f ABX 3 6 #xFF]
  [ISB #f ABY 3 6 #xFB]
  [LAX #f ZP0 2 3 #xA7]
  [LAX #f ZPY 2 4 #xB7]
  [LAX #f IZX 2 6 #xA3]
  [LAX #f IZY 2 5 #xB3]
  [LAX #f ABS 3 4 #xAF]
  [LAX #f ABY 3 4 #xBF]
  [LAX #f IMM 2 2 #xAB] ; "highly unstable (results are not predictable on some machines)" per http://www.oxyron.de/html/opcodes02.html
  [NOP #f ZP0 2 3 #x04 #x44 #x64]
  [NOP #f ABS 3 4 #x0C]
  [NOP #f ZPX 2 4 #x14 #x34 #x54 #x74 #xD4 #xF4]
  [NOP #f IMM 2 2 #x80 #x82 #x89 #xC2 #xE2]
  [NOP #f ABX 3 4 #x1C #x3C #x5C #x7C #xDC #xFC]
  [SAX #f ZP0 2 3 #x87]
  [SAX #f ZPY 2 4 #x97]
  [SAX #f IZX 2 6 #x83]
  [SAX #f ABS 3 4 #x8F]
  [SLO #f ZP0 2 5 #x07]
  [SLO #f ZPX 2 6 #x17]
  [SLO #f IZX 2 8 #x03]
  [SLO #f IZY 2 7 #x13]
  [SLO #f ABS 3 6 #x0F]
  [SLO #f ABX 3 6 #x1F]
  [SLO #f ABY 3 6 #x1B]
  [RLA #f ZP0 2 5 #x27]
  [RLA #f ZPX 2 6 #x37]
  [RLA #f IZX 2 8 #x23]
  [RLA #f IZY 2 7 #x33]
  [RLA #f ABS 3 6 #x2F]
  [RLA #f ABX 3 6 #x3F]
  [RLA #f ABY 3 6 #x3B]
  [SRE #f ZP0 2 5 #x47]
  [SRE #f ZPX 2 6 #x57]
  [SRE #f IZX 2 8 #x43]
  [SRE #f IZY 2 7 #x53]
  [SRE #f ABS 3 6 #x4F]
  [SRE #f ABX 3 6 #x5F]
  [SRE #f ABY 3 6 #x5B]
  [RRA #f ZP0 2 5 #x67]
  [RRA #f ZPX 2 6 #x77]
  [RRA #f IZX 2 8 #x63]
  [RRA #f IZY 2 7 #x73]
  [RRA #f ABS 3 6 #x6F]
  [RRA #f ABX 3 6 #x7F]
  [RRA #f ABY 3 6 #x7B]
  )

{begin ; Addressing Modes

  (define-mode (ABS)
    (let* ([lo : Fixnum (cpu-read PC)]
           [hi : Fixnum (cpu-read (fx+ 1 PC))]
           [result (fxior (fxlshift hi 8) lo)])
      (SET! PC (fx+ 2 PC))
      (values result 0)))

  (define-mode (IMM)
    (let ([result PC])
      (PC (fx+ 1 PC))
      (values result 0)))

  (define-mode (ZP0)
    (let ([result (cpu-read PC)])
      (PC (fx+ 1 PC))
      (values (fxand result #xFF) 0)))

  (define-mode (IMP)
    ; I think OLC uses the A register here to support the "Accumulator" mode
    ; which is used by the shifts... but I will just use zeros
    (values 0 0))

  (define-mode (REL)
    (let ([addr (cpu-read PC)]
          ; The branch instructions can return +1 or +2 extra-cycles
          [extra-cycles-mask #xFF])
      (PC (fx+ 1 PC))
      (values addr extra-cycles-mask)))

  (define-mode (IZX)
    (let* ([pc PC]
           [x X]
           [base (cpu-read pc)]
           [lo (cpu-read (fxand #xFF (fx+ base x)))]
           [hi (cpu-read (fxand #xFF (fx+ base (fx+ x 1))))]
           [addr (fxior lo (fxlshift hi 8))])
      (PC (fx+ 1 pc))
      (values addr 0)))

  (define-mode (IZY)
    (let* ([pc PC]
           [y Y]
           [base (cpu-read pc)]
           [lo (cpu-read base)]
           [hi (cpu-read (fxand #xFF (fx+ 1 base)))]
           [hi (fxlshift hi 8)] ; shift `hi` immediately
           [addr (fxior lo hi)]
           [addr (fxand #xFFFF (fx+ addr y))]
           ; now test whether the high byte of `addr` still matches `hi`
           [extra-cycle (if (fx= hi (fxand #xFF00 addr))
                            0
                            1)])
      (PC (fx+ 1 pc))
      (values addr extra-cycle)))

  (define-mode (IND)
    (let* ([pc PC]
           [pointer-lo (cpu-read pc)]
           [pointer-hi (cpu-read (fx+ 1 pc))]
           [pointer (fxior pointer-lo (fxlshift pointer-hi 8))]
           [addr-lo (cpu-read pointer)]
           [addr-hi (if (fx= pointer-lo #xFF)
                        (cpu-read (fxand pointer #xFF00))
                        (cpu-read (fx+ 1 pointer)))]
           [addr (fxior addr-lo (fxlshift addr-hi 8))])
      (PC (fx+ 1 pc))
      (values addr 0)))

  (define-syntax-rule (define-AB* [id reg] ...)
    (begin (define-mode (id)
             (let* ([pc PC]
                    [lo (cpu-read pc)]
                    [hi (cpu-read (fx+ 1 pc))]
                    [hi (fxlshift hi 8)]
                    [addr (fxior lo hi)]
                    [addr (fxand #xFFFF (fx+ addr reg))]
                    ; test whether `hi` still matches the high byte of `addr`
                    [extra-cycle (if (fx= hi (fxand #xFF00 addr))
                                     0
                                     1)])
               (PC (fx+ 2 PC))
               (values addr extra-cycle)))
           ...))
  (define-AB* [ABX X] [ABY Y])

  (define-syntax-rule (define-ZP* [id reg] ...)
    (begin (define-mode (id)
             (let* ([pc PC]
                    [addr (cpu-read pc)]
                    ; always page 0 (mask the high byte off)
                    [addr (fxand #x00FF (fx+ reg addr))])
               (PC (fx+ 1 pc))
               (values addr 0)))
           ...))
  (define-ZP* [ZPX X] [ZPY Y])

  ; end Addressing Modes
  }

(define-syntax-rule (step)
  (let ([opcode (cpu-read PC)])
    (PC (fx+ 1 PC))
    (let ([cycles (ann (emulate-one-instruction opcode)
                       Fixnum)])
      cycles)))

(define-syntax-rule (reset)
  (let ([lo (cpu-read #xFFFC)]
        [hi (cpu-read #xFFFD)])
    (SET! PC (fxior lo (fxlshift hi 8)))
    (SET! A 0)
    (SET! X 0)
    (SET! Y 0)
    (SET! SP #xFD)
    (SET! P (Flag.Unused #:mask))
    8 ; cycles
    ))

(define-syntax-rule (nmi)
  (let ([bits-to-set (ufxior* (Flag.B #:mask)
                              (Flag.I #:mask)
                              (Flag.Unused #:mask))])
    (cpu-write (ufx+ #x100 SP) (ufxand #xFF (ufxrshift PC 8)))
    (cpu-write (ufx+ #x0FF SP) (ufxand #xFF PC))
    (SET! P (ufxior P bits-to-set))
    (cpu-write (ufx+ #xFE SP) P)
    (SET! SP (ufx- SP 3))
    (let ([pc-lo (cpu-read #xFFFA)]
          [pc-hi (cpu-read #xFFFB)])
      (SET! PC (ufxior pc-lo (ufxlshift pc-hi 8)))
      8 ; cycles
      )))

(define-syntax (parameterize-registers stx)
  (syntax-case stx ()
    [(parameterize ([a b] ...) body ...)
     (with-syntax ([ooo (quote-syntax ...)])
       #'(syntax-parameterize ([a (lambda (stx)
                                    (syntax-case stx ()
                                      [(id val)
                                       #'(set! b val)]
                                      [id
                                       (identifier? #'id)
                                       #'b]
                                      [else
                                       #`(raise-syntax-error #f "Bad use?")]
                                      ))]
                               ...)
           body ...))]))

(module+ test
  (define .PC : Fixnum #xC000)
  (define .A : Fixnum 0)
  (define .X : Fixnum 0)
  (define .Y : Fixnum 0)
  (define .SP : Fixnum #xFD)
  (define .ST : Fixnum #x24)
  (define good-log (parse-reference-log))
  (define cycles : Fixnum 7)

  (define-syntax-rule (display* x ...)
    (begin (display x) ...))

  (define-syntax-rule (check-all-equal? [actual expected] ...)
    (begin (check-equal? actual expected)
           ...
           (and (equal? actual expected)
                ...)))

  ; Set up memory like this:
  ;   0000 - 7FFF general purpose RAM
  ;   8000 - BFFF nestest.nes
  ;   C000 - FFFF nestest.nes again
  ; I'll make the whole range writeable, that's probably not accurate...
  (define nestest-bytes (create-nestest-bytes))
  (define ram (make-bytes #x8000))
  (: .read (-> Fixnum Fixnum))
  (define (.read addr)
    (if (addr . fx< . #x8000)
        (bytes-ref ram addr)
        (bytes-ref nestest-bytes (fxmodulo addr #x4000))))
  (: .write (-> Fixnum Fixnum Any))
  (define (.write addr value)
    (if (addr . fx< . #x8000)
        (bytes-set! ram addr value)
        (bytes-set! nestest-bytes (fxmodulo addr #x4000) value)))

  ; Wire up syntax parameters and emulate
  (parameterize-registers
   ([PC .PC]
    [A .A]
    [X .X]
    [Y .Y]
    [SP .SP]
    [P .ST])
   (syntax-parameterize
       ([cpu-read (lambda (stx)
                    (syntax-case stx ()
                      [(_ arg ...) #'(.read arg ...)]))]
        [cpu-write (lambda (stx)
                     (syntax-case stx ()
                       [(_ arg ...) #'(.write arg ...)]))])
     ; Make sure the first instruction is where it should be
     (check-equal? (cpu-read #xC000) #x4C)
     (check-equal? (cpu-read #x8000) #x4C)
     ; Disable log for speed
     (define log? #f)
     ; Make sure these compile
     (define (proc-step) (step))
     (define (proc-reset) (reset))
     (define (proc-nmi) (nmi))
     ; Run until PC reaches last line of known good log
     (let loop ()
       (when log?
         (display (~h .PC 4)))
       (let* ([disassembly-port (open-output-string)]
              [orig-PC PC]
              [bytes (get-instruction-bytes orig-PC .read)]
              [byte-string (string-join (map ~h bytes) " ")]
              [a-string (format "A:~a" (~h .A))]
              [x-string (format "X:~a" (~h .X))]
              [y-string (format "Y:~a" (~h .Y))]
              [p-string (format "P:~a" (~h .ST))]
              [sp-string (format "SP:~a" (~h .SP))]
              [cyc-string (format "CYC:~a" cycles)])
         (parameterize ([current-output-port disassembly-port])
           (let ([cyc (proc-step)])
             (set! cycles (fx+ cycles cyc))))
         (define disassembly (get-output-string disassembly-port))
         (define log-item (car good-log))
         (set! good-log (cdr good-log))
         (define expected-disassembly (third log-item))
         (define all-good
           (check-all-equal? [(~h orig-PC 4) (first log-item)]
                             [byte-string (second log-item)]
                             [a-string (fourth log-item)]
                             [x-string (fifth log-item)]
                             [y-string (sixth log-item)]
                             [p-string (seventh log-item)]
                             [sp-string (eighth log-item)]
                             [cyc-string (tenth log-item)]))
         (when (or log? (not all-good))
           (display* "  " (~a byte-string #:width 10))
           ; I don't understand what the "good log" means when it says something like
           ;     STA $01 = 00
           ; What does the 00 indicate here? It is neither the current nor previous value of A...
           ; Oh well, let's just send these to the error port and continue on.
           (display (~a disassembly #:width 20) (if (equal? disassembly expected-disassembly)
                                                    (current-output-port)
                                                    (current-error-port)))
           (display* " " a-string
                     " " x-string
                     " " y-string
                     " " p-string
                     " " sp-string
                     " " cyc-string)
           (displayln ""))
         (when (and all-good
                    (not (fx= .PC #xC66E)))
           (loop))))))
  )
