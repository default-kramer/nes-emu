#lang typed/racket

(provide expand-opcode-table
         (struct-out operation) Operation
         get-opcode-details
         )

(define-for-syntax the-table
  #'(; == Branches ==
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
     ))

(define-for-syntax (rebind-table ctx)
  (define (handle-one stx)
    (let ([e (syntax-e stx)])
      (cond
        [(symbol? e)
         (datum->syntax ctx e stx stx stx)]
        [(list? e)
         (datum->syntax stx (map handle-one e) stx stx stx)]
        [(pair? e)
         (datum->syntax stx (cons (handle-one (car e))
                                  (handle-one (cdr e)))
                        stx stx stx)]
        [else stx])))
  (handle-one the-table))

(define-syntax (expand-opcode-table stx)
  (syntax-case stx ()
    [(_ expander stuff ...)
     (quasisyntax/loc stx
       (expander stuff ... #,(rebind-table stx)))]))


(struct operation ([mnemonic : Symbol]
                   [opcode : (U #f Fixnum)]
                   [mode : Symbol]
                   [bytes : Fixnum]
                   [base-cycles : Fixnum]
                   [unofficial-opcodes : (Listof Fixnum)])
  #:transparent #:type-name Operation)

(define-syntax-rule (make-opcode-lookup
                     op-arg
                     ([handler opcode mode bytes base-cycles unofficial-opcode ...]
                      ...))
  (case op-arg
    [(opcode unofficial-opcode ...)
     (operation 'handler opcode 'mode bytes base-cycles (list unofficial-opcode ...))]
    ...
    [else #f]))

(: get-opcode-details (-> Fixnum (U #f Operation)))
(define (get-opcode-details opcode)
  (expand-opcode-table make-opcode-lookup opcode))
