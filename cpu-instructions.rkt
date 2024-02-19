#lang typed/racket

; Remaining cleanup:
; * Possibly improve the 4 occurrences of Accumulator addressing mode,
;   in which the handler looks at the opcode.
;   Update documentation to clarify that the addressing mode proc
;   returns an "address" and not an "operand".
; * Maybe test if returning (list address extra-cycles-mask) is faster
;   than returning (values address extra-cycles-mask).

(require racket/stxparam
         "ufx.rkt"
         (only-in "doodling.rkt" get-branch-dest))

; == Explanation of cycle counting ==
; See https://www.nesdev.org/obelisk-6502-guide/reference.html
; Many opcodes have a constant cycle count, such as opcode $69
; (ADC IMM) which always takes 2 cycles no matter what.
; But other opcodes have a variable cycle count that can only be
; determined at runtime, so we need a strategy to handle this.
; 
; The strategy is:
;   BaseCycles is defined per opcode
;   ExtraCycleMask is returned from the addressing mode
;   ExtraCycles is returned from the instruction handler
;   ActualCycles = BaseCycles + (ExtraCycles & ExtraCyclesMask)
;
; For branch instructions (such as BCC), the spec says
;   "Cycles: 2 (+1 if branch succeeds, +2 if to a new page)"
; All branches support only the REL addressing mode, and the REL
; mode is not used for any other instructions.
; So the REL mode unconditionally returns ExtraCycleMask=0xFF
; and the branch instructions return +0 or +1 or +2 ExtraCycles.
;
; We also have extra cycles whenever the spec says
;   "Cycles: [N] (+1 if page crossed)"
; There are two important things to note about this:
; 1) This only happens for 3 addressing modes: ABX, ABY, and IZY.
; 2) Whenever some instruction is sensitive to one of those 3 addressing
;    modes, it is sensitive to all 3 of them.
; So those 3 addressing modes will return ExtraCyclesMask=1
; when a page is crossed, and ExtraCyclesMask=0 otherwise.
; And all the instructions which are sensitive to this extra cycle will
; unconditionally return ExtraCycles=1. Here is a constant for that purpose:
(define respect-mode-cycle 1)
; This means that when an instruction handler returns this value,
; it will only add an extra cycle when the addressing mode agrees
; that an extra cycle is necessary.
;
; Addressing Mode Recap:
; IMM - only for branches, always returns ExtraCycleMask=0xFF
; ABX, ABY, IZY - returns ExtraCycleMask=1 when a page is crossed, 0 otherwise
; all others - always returns ExtraCycleMask=0

(define no-extra-cycles 0)

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

#;(module+ test
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

(define-syntax-rule (define-handlers ooo def-handler-full define-mode WISH)
  ; The handlers within can be reified as macros or as procedures
  ; depending on what the caller provides.
  {begin
    (define-registers WISH PC A X Y SP P)
    (define-syntax-rule (cpu-write addr value)
      (WISH #:write addr value))
    (define-syntax-rule (cpu-read addr)
      (WISH #:read addr))
    ; == do not directly touch WISH beyond this point ==

    (define-syntax-rule (set-flag flag)
      (set! P (ufxior P (flag #:mask))))

    (define-syntax-rule (clear-flag flag)
      (set! P (ufxand P (flag #:antimask))))

    (define-syntax-rule (put-flag id test)
      (if test
          (set-flag id)
          (clear-flag id)))

    (define-syntax-rule (get-flag flag)
      (not (ufx= 0 (ufxand P (flag #:mask)))))

    (define-syntax-rule (setZN byte) ; helper to set Z and N flags
      (begin (put-flag Flag.Z (ufx= 0 byte))
             (put-flag Flag.N (ufx= #x80 (ufxand #x80 byte)))))

    ; Most of the handlers ignore the opcode:
    (define-syntax-rule (def-handler (id arg) body ooo)
      (def-handler-full (id arg ignored-opcode) body ooo))

    (def-handler (JMP addr)
      (set! PC addr)
      no-extra-cycles)

    (def-handler (JSR addr)
      (set! PC (ufx+ PC -1))
      (cpu-write (ufx+ #x0100 SP)
                 (ufxand #xFF (ufxrshift PC 8)))
      (set! SP (ufx+ SP -1))
      (cpu-write (ufx+ #x0100 SP)
                 (ufxand #xFF PC))
      (set! SP (ufx+ SP -1))
      (set! PC addr)
      no-extra-cycles)

    (def-handler (RTS addr)
      (let* ([sp SP]
             [a (cpu-read (ufx+ #x101 sp))]
             [b (cpu-read (ufx+ #x102 sp))])
        (set! SP (ufx+ 2 sp))
        (set! PC (ufx+ 1 (ufxior a (ufxlshift b 8))))
        no-extra-cycles))

    (def-handler (RTI addr)
      (let* ([sp SP]
             [sts (cpu-read (ufx+ #x101 sp))]
             ; I guess RTI sets the U flag?
             [sts (ufxior sts (Flag.Unused #:mask))]
             [pc-lo (cpu-read (ufx+ #x102 sp))]
             [pc-hi (cpu-read (ufx+ #x103 sp))])
        (set! P sts)
        (set! SP (ufx+ 3 sp))
        (set! PC (ufxior pc-lo (ufxlshift pc-hi 8)))
        no-extra-cycles))

    (def-handler (PHP addr)
      (cpu-write (ufx+ #x100 SP)
                 ; I don't see this in the reference, but according to OneLoneCoder
                 ; "Break flag is set to 1 before push" so let's assume he's correct
                 (ufxior P (Flag.B #:mask)))
      ; And now the B flag is cleared apparently?
      (clear-flag Flag.B)
      (set! SP (ufx+ SP -1))
      no-extra-cycles)

    (def-handler (PHA addr)
      (cpu-write (ufx+ #x100 SP) A)
      (set! SP  (ufx+ SP -1))
      no-extra-cycles)

    (def-handler (PLA addr)
      (let ([byte (cpu-read (ufx+ #x101 SP))])
        (set! SP (ufx+ 1 SP))
        (set! A byte)
        (setZN byte)
        no-extra-cycles))

    (def-handler (PLP addr)
      (let* ([sp SP]
             [byte (cpu-read (ufx+ #x101 sp))]
             ; Set the U bit
             [byte (ufxior byte (Flag.Unused #:mask))]
             ; WTF - I'm just guessing here... Do we alwyas clear the B flag here, or do I have some other bug?
             ; Is this related to OLC "Break flag is set to 1 before push" on the PHP?
             [byte (ufxand byte (Flag.B #:antimask))])
        (set! SP (ufx+ 1 SP))
        (set! P byte)
        no-extra-cycles))

    (define-syntax-rule (define-LD* [id reg ooo] ooo)
      (begin
        (def-handler (id addr)
          (let ([byte (cpu-read addr)])
            (begin (set! reg byte)
                   ooo)
            (setZN byte)
            respect-mode-cycle))
        ooo))
    (define-LD* [LDX X] [LDY Y] [LDA A]
      [LAX A X] ; unofficial, sets both A and X
      )

    (def-handler (AND addr)
      (let* ([byte (cpu-read addr)]
             [a (ufxand A byte)])
        (set! A a)
        (setZN a)
        respect-mode-cycle))

    (def-handler (ORA addr)
      (let* ([byte (cpu-read addr)]
             [a (ufxior A byte)])
        (set! A a)
        (setZN a)
        respect-mode-cycle))

    (def-handler (EOR addr)
      (let* ([byte (cpu-read addr)]
             [a (ufxxor A byte)])
        (set! A a)
        (setZN a)
        respect-mode-cycle))

    ; Supports ADC and SBC (and ISB)
    (define-syntax-rule (adc-helper TODO-VAL)
      (let* ([a A]
             [val TODO-VAL]
             ; Because Flag.C is the lowest bit, masking it will produce 0 or 1
             [carry (ufxand (Flag.C #:mask) P)]
             [result (ufx+ a (ufx+ val carry))]
             ; TODO copied from OneLoneCoder, can this be right?
             [v-magic (ufxand (ufxnot (ufxxor a val))
                              (ufxxor a result))]
             [v-magic (ufxand v-magic #x80)])
        (put-flag Flag.C (ufx> result 255))
        (put-flag Flag.V (not (ufx= 0 v-magic)))
        (set! A (ufxand #xFF result))
        (setZN A)
        respect-mode-cycle))

    (define-syntax-rule (sbc-helper val)
      ; Invert the bottom 8 bits and reuse the ADC logic
      (let ([val (ufxand #xFFFF (ufxxor #xFF val))])
        (adc-helper val)))

    (def-handler (ADC addr)
      (let ([mem (cpu-read addr)])
        (adc-helper mem)))

    (def-handler (SBC addr)
      (let ([mem (cpu-read addr)])
        (sbc-helper mem)))

    ; increment/decrement
    (define-syntax-rule (define-*crements [id register amount] ooo)
      (begin
        (def-handler (id addr)
          (let* ([result (ufx+ register amount)]
                 [result (ufxand result #xFF)])
            (set! register result)
            (setZN result)
            no-extra-cycles))
        ooo))
    (define-*crements [INX X 1] [INY Y 1] [DEX X -1] [DEY Y -1])

    (def-handler (INC addr)
      (let* ([byte (cpu-read addr)]
             [raw (ufx+ 1 byte)]
             [byte (ufxand #xFF raw)])
        (cpu-write addr byte)
        (setZN byte)
        no-extra-cycles))

    (def-handler (DEC addr)
      (let* ([byte (cpu-read addr)]
             [raw (ufx- byte 1)]
             [byte (ufxand #xFF raw)])
        (cpu-write addr byte)
        (setZN byte)
        no-extra-cycles))

    (define-syntax-rule (define-comparisons [id register] ooo)
      (begin
        (def-handler (id addr)
          (let* ([byte (cpu-read addr)]
                 [diff (ufx- register byte)])
            (put-flag Flag.C (ufx>= diff 0))
            (setZN diff)
            ; Only CMP *needs* to respect the mode cycle.
            ; For CPX/CPY the mode cycle will always be zero so it doesn't matter.
            respect-mode-cycle))
        ooo))
    (define-comparisons [CMP A] [CPX X] [CPY Y])

    ; DCP is unofficial, something like "DEC then CMP".
    ; Here's my best guess, hope the tests tell me if it's wrong:
    (def-handler (DCP addr)
      (let* ([byte (cpu-read addr)]
             [byte (ufxand #xFF (ufx- byte 1))]
             [diff (ufx- A byte)])
        (cpu-write addr byte)
        (put-flag Flag.C (ufx>= diff 0))
        (setZN diff)
        respect-mode-cycle))

    ; ISB is unofficial
    (def-handler (ISB addr)
      (let* ([byte (cpu-read addr)]
             [raw (ufx+ 1 byte)]
             [byte (ufxand #xFF raw)])
        (cpu-write addr byte)
        (sbc-helper byte)))

    ; SLO unofficial, something like "ASL then ORA"
    (def-handler (SLO addr)
      (let* ([byte (cpu-read addr)]
             ; If highest bit was set, carry gets set
             [_ (put-flag Flag.C (ufx= #x80 (ufxand #x80 byte)))]
             [raw (ufxlshift byte 1)]
             [byte (ufxand #xFF raw)]
             [a (ufxior A byte)])
        (cpu-write addr byte)
        (set! A a)
        (setZN a)
        respect-mode-cycle))

    ; RLA unofficial, something like "ROL then AND"
    (def-handler (RLA addr)
      (let* ([byte (cpu-read addr)]
             ; Need to read C *before* we overwrite it!
             [raw (ufxlshift byte 1)]
             [raw (if (get-flag Flag.C)
                      (ufxior 1 raw)
                      raw)]
             ; If highest bit was set, carry gets set
             [_ (put-flag Flag.C (ufx= #x80 (ufxand #x80 byte)))]
             ; Write back to memory *before* doing the AND
             [byte (ufxand #xFF raw)]
             [_ (cpu-write addr byte)]
             ; Finally, do the AND
             [byte (ufxand A byte)])
        (set! A byte)
        (setZN byte)
        respect-mode-cycle))

    ; SRE unofficial, something like "LSR then EOR"
    (def-handler (SRE addr)
      (let* ([byte (cpu-read addr)]
             ; Here don't need to read C, so we can write it immediately
             [_ (put-flag Flag.C (ufx= 1 (ufxand 1 byte)))]
             ; Shift and write to memory
             [byte (ufxrshift byte 1)]
             [_ (cpu-write addr byte)]
             ; Now do the EOR
             [byte (ufxxor A byte)])
        (set! A byte)
        (setZN byte)
        respect-mode-cycle))

    ; RRA unofficial, something like "ROR then ADC"
    (def-handler (RRA addr)
      (let* ([orig-byte (cpu-read addr)]
             ; Need to read the C flag before we write it
             [byte (ufxior (ufxrshift orig-byte 1)
                           ; C flag is first bit of P, so shift it left 7
                           (ufxlshift (ufxand 1 P) 7))])
        (put-flag Flag.C (ufx= 1 (ufxand 1 orig-byte)))
        (cpu-write addr byte)
        (adc-helper byte)))

    (define-syntax-rule (define-ST* [id val-expr] ooo)
      (begin
        (def-handler (id addr)
          (let ([val val-expr])
            (cpu-write addr val)
            no-extra-cycles))
        ooo))

    (define-ST* [STX X] [STY Y] [STA A]
      [SAX (ufxand A X)] ; unofficial, stores A&X
      )

    (def-handler (BIT addr)
      (let ([byte (cpu-read addr)])
        (put-flag Flag.Z (ufx= 0 (ufxand A byte)))
        (put-flag Flag.N (not (ufx= 0 (ufxand byte 128))))
        (put-flag Flag.V (not (ufx= 0 (ufxand byte 64))))
        no-extra-cycles))

    (def-handler (BRK addr)
      (set! PC (ufx+ 1 PC))
      (set-flag Flag.I)
      (cpu-write (ufx+ #x100 SP) (ufxand #xFF (ufxrshift PC 8)))
      (cpu-write (ufx+ #x0FF SP) (ufxand #xFF PC))
      (set-flag Flag.B)
      (cpu-write (ufx+ #x0FE SP) P)
      (set! SP (ufx- SP 3))
      (clear-flag Flag.B)
      (let ([hi (cpu-read #xFFFE)]
            [lo (cpu-read #xFFFF)])
        (set! PC (ufxior lo (ufxlshift hi 8))))
      no-extra-cycles)

    (def-handler (NOP addr)
      ; unofficial NOPs have to respect the mode cycle
      respect-mode-cycle)

    (define-syntax-rule (define-setters [id flag] ooo)
      (begin
        (def-handler (id addr)
          (set-flag flag)
          no-extra-cycles)
        ooo))
    (define-setters [SEC Flag.C] [SED Flag.D] [SEI Flag.I])

    (define-syntax-rule (define-clearers [id flag] ooo)
      (begin
        (def-handler (id addr)
          (clear-flag flag)
          no-extra-cycles)
        ooo))
    (define-clearers [CLC Flag.C] [CLD Flag.D] [CLI Flag.I] [CLV Flag.V])

    (define-syntax-rule (define-transfers [id src dst] ooo)
      (begin
        (def-handler (id addr)
          (let ([byte src])
            (set! dst byte)
            (setZN byte)
            no-extra-cycles))
        ooo))
    (define-transfers [TAX A X] [TAY A Y] [TXA X A] [TYA Y A] [TSX SP X])
    ; Oops - TXS does not perform a setZN, so here it is:
    (def-handler (TXS addr)
      (let ([byte X])
        (set! SP byte)
        no-extra-cycles))

    (define-syntax-rule (define-branch ID condition)
      ; All the branches support REL mode only, and the timing is always
      ;   "2 (+1 if branch succeeds, +2 if to a new page)"
      ; We assume that base-cycles will be 2 and extra-cycles-mask
      ; will be xFF or something, so that when we return +1 or +2
      ; it will all add up correctly.
      ; Tricky signed/unsigned math is handled by `get-branch-dest`.
      (def-handler (ID .addr)
        (let* ([pc PC]
               [addr .addr]
               [addr_abs (get-branch-dest pc addr)])
          (if condition
              (let ([cycles (if (ufx= (ufxand #xFF00 addr_abs)
                                      (ufxand #xFF00 pc))
                                1 ; for 3 total cycles
                                2 ; for 4 total cycles
                                )])
                (set! PC addr_abs)
                cycles)
              0 ; for 2 total cycles
              ))))
    (define-branch BCS (get-flag Flag.C))
    (define-branch BCC (not (get-flag Flag.C)))
    (define-branch BEQ (get-flag Flag.Z))
    (define-branch BNE (not (get-flag Flag.Z)))
    (define-branch BVS (get-flag Flag.V))
    (define-branch BVC (not (get-flag Flag.V)))
    (define-branch BMI (get-flag Flag.N))
    (define-branch BPL (not (get-flag Flag.N)))


    (def-handler-full (LSR addr opcode)
      (let* ([raw (case opcode
                    [(#x4A) A]
                    [else (cpu-read addr)])]
             [byte (ufxrshift raw 1)])
        ; If lowest bit was set, carry gets set
        (put-flag Flag.C (ufx= 1 (ufxand 1 raw)))
        (setZN byte)
        (case opcode
          [(#x4A) (begin ; Accumulator addressing mode
                    (set! A byte))]
          [else (begin
                  (cpu-write addr byte))])
        no-extra-cycles))

    (def-handler-full (ASL addr opcode)
      (let* ([byte (case opcode
                     [(#x0A) A]
                     [else (cpu-read addr)])]
             [raw (ufxlshift byte 1)]
             [byte (ufxand #xFF raw)])
        ; If highest bit was set, carry gets set
        (put-flag Flag.C (ufx= #x100 (ufxand #x100 raw)))
        (setZN byte)
        (case opcode
          [(#x0A) (begin ; Accumulator addressing mode
                    (set! A byte))]
          [else (begin
                  (cpu-write addr byte))])
        no-extra-cycles))

    (def-handler-full (ROR addr opcode)
      (let* ([raw (case opcode
                    [(#x6A) A]
                    [else (cpu-read addr)])]
             [byte (ufxrshift raw 1)]
             [byte (ufxior byte (if (get-flag Flag.C)
                                    #x80
                                    0))])
        ; If highest bit was set, carry gets set
        (put-flag Flag.C (ufx= 1 (ufxand 1 raw)))
        (setZN byte)
        (case opcode
          [(#x6A) (begin ; Accumulator addressing mode
                    (set! A byte))]
          [else (begin
                  (cpu-write addr byte))])
        no-extra-cycles))

    (def-handler-full (ROL addr opcode)
      (let* ([byte (case opcode
                     [(#x2A) A]
                     [else (cpu-read addr)])]
             [raw (ufxlshift byte 1)]
             [raw (if (get-flag Flag.C)
                      (ufxior 1 raw)
                      raw)]
             [byte (ufxand raw #xFF)])
        (put-flag Flag.C (ufx= #x100 (ufxand #x100 raw)))
        (setZN byte)
        (case opcode
          [(#x2A) (begin ; accumulator
                    (set! A byte))]
          [else (begin
                  (cpu-write addr byte))])
        no-extra-cycles))

    ; These (reset and nmi) aren't instruction handlers
    ; but it's convenient to construct them the same way
    (def-handler-full (reset ignored-addr ignored-opcode)
      (let ([lo (cpu-read #xFFFC)]
            [hi (cpu-read #xFFFD)])
        (set! PC (ufxior lo (ufxlshift hi 8)))
        (set! A 0)
        (set! X 0)
        (set! Y 0)
        (set! SP #xFD)
        (set! P (Flag.Unused #:mask))
        8 ; cycles
        ))

    (def-handler-full (nmi ignored-addr ignored-opcode)
      (let ([bits-to-set (ufxior* (Flag.B #:mask)
                                  (Flag.I #:mask)
                                  (Flag.Unused #:mask))])
        (cpu-write (ufx+ #x100 SP) (ufxand #xFF (ufxrshift PC 8)))
        (cpu-write (ufx+ #x0FF SP) (ufxand #xFF PC))
        (set! P (ufxior P bits-to-set))
        (cpu-write (ufx+ #xFE SP) P)
        (set! SP (ufx- SP 3))
        (let ([pc-lo (cpu-read #xFFFA)]
              [pc-hi (cpu-read #xFFFB)])
          (set! PC (ufxior pc-lo (ufxlshift pc-hi 8)))
          8 ; cycles
          )))

    ; end handlers, begin addressing modes
    (define-mode (ABS)
      (let* ([lo : Fixnum (cpu-read PC)]
             [hi : Fixnum (cpu-read (ufx+ 1 PC))]
             [result (ufxior (ufxlshift hi 8) lo)])
        (set! PC (ufx+ 2 PC))
        (values result 0)))

    (define-mode (IMM)
      (let ([result PC])
        (set! PC (ufx+ 1 PC))
        (values result 0)))

    (define-mode (ZP0)
      (let ([result (cpu-read PC)])
        (set! PC (ufx+ 1 PC))
        (values (ufxand result #xFF) 0)))

    (define-mode (IMP)
      ; I think OLC uses the A register here to support the "Accumulator" mode
      ; which is used by the shifts... but I will just use zeros
      (values 0 0))

    (define-mode (REL)
      (let ([addr (cpu-read PC)]
            ; The branch instructions can return +1 or +2 extra-cycles
            [extra-cycles-mask #xFF])
        (set! PC (ufx+ 1 PC))
        (values addr extra-cycles-mask)))

    (define-mode (IZX)
      (let* ([pc PC]
             [x X]
             [base (cpu-read pc)]
             [lo (cpu-read (ufxand #xFF (ufx+ base x)))]
             [hi (cpu-read (ufxand #xFF (ufx+ base (ufx+ x 1))))]
             [addr (ufxior lo (ufxlshift hi 8))])
        (set! PC (ufx+ 1 pc))
        (values addr 0)))

    (define-mode (IZY)
      (let* ([pc PC]
             [y Y]
             [base (cpu-read pc)]
             [lo (cpu-read base)]
             [hi (cpu-read (ufxand #xFF (ufx+ 1 base)))]
             [hi (ufxlshift hi 8)] ; shift `hi` immediately
             [addr (ufxior lo hi)]
             [addr (ufxand #xFFFF (ufx+ addr y))]
             ; now test whether the high byte of `addr` still matches `hi`
             [extra-cycle (if (ufx= hi (ufxand #xFF00 addr))
                              0
                              1)])
        (set! PC (ufx+ 1 pc))
        (values addr extra-cycle)))

    (define-mode (IND)
      (let* ([pc PC]
             [pointer-lo (cpu-read pc)]
             [pointer-hi (cpu-read (ufx+ 1 pc))]
             [pointer (ufxior pointer-lo (ufxlshift pointer-hi 8))]
             [addr-lo (cpu-read pointer)]
             [addr-hi (if (ufx= pointer-lo #xFF)
                          (cpu-read (ufxand pointer #xFF00))
                          (cpu-read (ufx+ 1 pointer)))]
             [addr (ufxior addr-lo (ufxlshift addr-hi 8))])
        (set! PC (ufx+ 1 pc))
        (values addr 0)))

    (define-syntax-rule (define-AB* [id reg] ooo)
      (begin (define-mode (id)
               (let* ([pc PC]
                      [lo (cpu-read pc)]
                      [hi (cpu-read (ufx+ 1 pc))]
                      [hi (ufxlshift hi 8)]
                      [addr (ufxior lo hi)]
                      [addr (ufxand #xFFFF (ufx+ addr reg))]
                      ; test whether `hi` still matches the high byte of `addr`
                      [extra-cycle (if (ufx= hi (ufxand #xFF00 addr))
                                       0
                                       1)])
                 (set! PC (ufx+ 2 PC))
                 (values addr extra-cycle)))
             ooo))
    (define-AB* [ABX X] [ABY Y])

    (define-syntax-rule (define-ZP* [id reg] ooo)
      (begin (define-mode (id)
               (let* ([pc PC]
                      [addr (cpu-read pc)]
                      ; always page 0 (mask the high byte off)
                      [addr (ufxand #x00FF (ufx+ reg addr))])
                 (set! PC (ufx+ 1 pc))
                 (values addr 0)))
             ooo))
    (define-ZP* [ZPX X] [ZPY Y])

    ; end Addressing Modes
    })

{module+ as-macros
  (provide PC A X Y SP P cpu-read cpu-write)

  (define-syntax-rule (define-registers id ...)
    (begin
      (define-syntax-parameter id
        (lambda (stx)
          (raise-syntax-error 'id "CPU not parameterized")))
      ...))

  ; Maybe we can transparently handle overflow in the setters?
  ; I think, for example, that (set! PC x) really means
  ; (set! PC (fxand x #xFFFF))
  ; PC - Program Counter
  ; A,X,Y - general purpose registers
  ; SP - stack pointer
  ; P - status flags (apparently "P" is for "Processor status")
  (define-registers PC A X Y SP P cpu-read cpu-write)
  (define-for-syntax (get-reg id)
    (if (syntax? id)
        (get-reg (syntax->datum id))
        (case id
          [(PC) #'PC]
          [(A)  #'A]
          [(X)  #'X]
          [(Y)  #'Y]
          [(SP) #'SP]
          [(P)  #'P]
          [else (error "bad register" id)])))

  (define-syntax (def-handler stx)
    (syntax-case stx ()
      [(_ (id operand opcode) body ...)
       #'(begin
           (provide id)
           (define-syntax-rule (id #:address operand #:opcode opcode)
             (let ([extra-cycles (ann (let () body ...) Fixnum)])
               extra-cycles)))]))

  (define-syntax (def-mode stx)
    (syntax-case stx ()
      [(_ (id) body ...)
       #'(begin
           (provide id)
           (define-syntax-rule (id)
             (begin body ...)))]))

  (define-syntax (wish stx)
    (syntax-case stx ()
      [(_ #:set! id val)
       (quasisyntax/loc stx
         (#,(get-reg #'id) val))]
      [(_ #:get id)
       (get-reg #'id)]
      [(_ #:write addr val)
       (syntax/loc stx
         (cpu-write addr val))]
      [(_ #:read addr)
       (syntax/loc stx
         (cpu-read addr))]))

  (define-handlers ... def-handler def-mode wish)
  }

{module+ struct-and-procs
  ; Each handler will also be provided
  (provide (struct-out cpu) CPU)

  (struct cpu ([PC : Fixnum]
               [A : Fixnum]
               [X : Fixnum]
               [Y : Fixnum]
               [SP : Fixnum]
               [P : Fixnum]
               [read-proc : (-> Fixnum Fixnum)]
               [write-proc : (-> Fixnum Fixnum Void)])
    #:mutable #:type-name CPU)

  #;(CPU operand opcode -> extra-cycles)
  (define-type InstructionHandler (-> CPU Fixnum Fixnum Fixnum))

  #;(CPU -> address extra-cycles-mask)
  (define-type AddressingMode (-> CPU (Values Fixnum Fixnum)))

  (define-syntax-parameter the-cpu
    (lambda (stx)
      (raise-syntax-error 'the-cpu "not parameterized" stx)))

  (define-syntax-rule (def-handler (id operand opcode) body ...)
    (begin (provide id)
           (: id InstructionHandler)
           (define (id cpu-arg operand opcode)
             (syntax-parameterize ([the-cpu (lambda (stx) #'cpu-arg)])
               (ann (let () body ...) Fixnum)))))

  (define-syntax-rule (def-mode (id) body ...)
    (begin (provide id)
           (: id AddressingMode)
           (define (id cpu-arg)
             (syntax-parameterize ([the-cpu (lambda (stx) #'cpu-arg)])
               body ...))))

  (define-syntax (wish stx)
    (syntax-case stx ()
      [(_ #:set! id val)
       (case (syntax-e #'id)
         [(PC) #'(set-cpu-PC! the-cpu val)]
         [(A)  #'(set-cpu-A!  the-cpu val)]
         [(X)  #'(set-cpu-X!  the-cpu val)]
         [(Y)  #'(set-cpu-Y!  the-cpu val)]
         [(SP) #'(set-cpu-SP! the-cpu val)]
         [(P)  #'(set-cpu-P!  the-cpu val)]
         [else (error "cannot set:" #'id)])]
      [(_ #:get id)
       (case (syntax-e #'id)
         [(PC) #'(cpu-PC the-cpu)]
         [(A)  #'(cpu-A  the-cpu)]
         [(X)  #'(cpu-X  the-cpu)]
         [(Y)  #'(cpu-Y  the-cpu)]
         [(SP) #'(cpu-SP the-cpu)]
         [(P)  #'(cpu-P  the-cpu)]
         [else (error "cannot get:" #'id)])]
      [(_ #:write addr val)
       (syntax/loc stx
         ((cpu-write-proc the-cpu) addr val))]
      [(_ #:read addr)
       (syntax/loc stx
         ((cpu-read-proc the-cpu) addr))]))

  (define-handlers ... def-handler def-mode wish)
  }

(module+ main
  (require (submod ".." struct-and-procs))
  (println (list JMP JSR)))
