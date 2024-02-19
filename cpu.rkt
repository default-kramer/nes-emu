#lang typed/racket

(provide CPU make-cpu cpu-step cpu-reset cpu-nmi
         cpu-get-state)

(require "nestest.rkt"
         "util.rkt"
         "ufx.rkt"
         "cpu-opcode-table.rkt"
         racket/stxparam)

(module+ test
  (require typed/rackunit))

; The following module reifies the CPU such that each addressing mode
; and each instruction handler is its own procedure.
{module struct-and-procs typed/racket
  (provide CPU make-cpu cpu-step cpu-reset cpu-nmi
           cpu-get-state)

  (require "cpu-opcode-table.rkt"
           "ufx.rkt"
           (submod "cpu-instructions.rkt" struct-and-procs))

  (define-syntax-rule (create-emulator
                       [#:emulate-one emu-one]
                       ([handler opcode mode byte-count base-cycles unofficial-opcode ...]
                        ...))
    (define-syntax-rule (emu-one op cpu)
      ; The opcode is passed in
      (case op
        [(opcode unofficial-opcode ...)
         (let-values ([([address : Fixnum] [extra-cycles-mask : Fixnum])
                       (mode cpu)])
           (let ([extra-cycles : Fixnum
                               (handler cpu address op)])
             ; Return total cycles
             (ufx+ base-cycles (ufxand extra-cycles-mask extra-cycles))))]
        ...
        [else (error (format "Invalid opcode: $~x ~a"
                             op cpu))])))

  (expand-opcode-table create-emulator [#:emulate-one emulate-one-instruction])

  (: make-cpu (-> (-> Fixnum Fixnum) (-> Fixnum Fixnum Void) CPU))
  (define (make-cpu cpu-read cpu-write)
    ; TODO these startup values are specific to nestest,
    ; what should they actually be?
    (cpu #xC000 ; PC
         0 ; A
         0 ; X
         0 ; Y
         #xFD ; SP
         #x24 ; ST
         cpu-read
         cpu-write))

  (: cpu-step (-> CPU Fixnum))
  (define (cpu-step cpu)
    (let* ([pc (cpu-PC cpu)]
           [opcode ((cpu-read-proc cpu) pc)])
      (set-cpu-PC! cpu (ufx+ 1 pc))
      (let ([cycles (ann (emulate-one-instruction opcode cpu)
                         Fixnum)])
        cycles)))

  (: cpu-reset (-> CPU Fixnum))
  (define (cpu-reset cpu)
    (reset cpu 0 0))

  (: cpu-nmi (-> CPU Fixnum))
  (define (cpu-nmi cpu)
    (nmi cpu 0 0))

  (: cpu-get-state (-> CPU (List Fixnum Fixnum Fixnum Fixnum Fixnum Fixnum)))
  (define (cpu-get-state cpu)
    (list (cpu-PC cpu)
          (cpu-A cpu)
          (cpu-X cpu)
          (cpu-Y cpu)
          (cpu-SP cpu)
          (cpu-P cpu)))

  ; end module struct-and-procs
  }

; The following module reifies the CPU such that there is a single large procedure
; which handles all opcodes. All addressing modes and instruction handlers
; are inlined via macros into this single procedure.
; My initial thought was that doing this would allow the compiler to
; optimize certain combinations when the mode does some unnecessary work
; that the handler never ends up using. But as I make the CPU more accurate,
; I'm realizing that there aren't very many such combinations.
;
; Right now, this approach is slower than structs-and-procs.
; These are the results of cpu-speed-test.rkt
;   struct-and-procs: 1750-1800 millis for 3000 runs
;   one-big-proc    : 1900-1975 millis for 3000 runs
; But it used to be faster
;   old-one-big-proc: 975-1025 millis for 3000 runs
; Yes, it used to be faster but it's not a fair comparison because
; it was a global singleton instance. It should be faster because
; * cpu-read and cpu-write were directly wired up to procedures
;   known at compile-time, as opposed to the current approach which
;   uses, for example, (cpu-read-proc the-cpu)
; * The current approach requires this bit of extra code to reflect any
;   changes back to the struct. I suppose as long as I need a struct
;   anyway, I should mutate the struct members directly instead
;   of my let-bound copies...
#;((set-cpu-PC! cpu pc)
   (set-cpu-A! cpu a)
   (set-cpu-X! cpu x)
   (set-cpu-Y! cpu y)
   (set-cpu-SP! cpu sp)
   (set-cpu-P! cpu p))
; Perhaps it will be possible to get some of that performance back,
; so I'll leave this macro approach here.
; But for now, I'm back to thinking that the cost of the PPU code
; will likely outweigh that of the CPU code.
{module one-big-proc typed/racket
  (provide CPU make-cpu cpu-step cpu-reset cpu-nmi
           cpu-get-state)

  (require "cpu-opcode-table.rkt"
           "ufx.rkt"
           "util.rkt"
           (submod "cpu-instructions.rkt" as-macros)
           racket/stxparam)

  (define-syntax-rule (create-emulator
                       [#:emulate-one emu-one]
                       ([handler opcode mode byte-count base-cycles unofficial-opcode ...]
                        ...))
    (define-syntax-rule (emu-one op)
      ; The opcode is passed in
      (case op
        [(opcode unofficial-opcode ...)
         (let-values ([([address : Fixnum] [extra-cycles-mask : Fixnum])
                       (mode)])
           (let ([extra-cycles
                  : Fixnum
                  (handler #:address address #:opcode op)])
             ; Return total cycles
             (ufx+ base-cycles (ufxand extra-cycles-mask extra-cycles))))]
        ...
        [else (error (format "Invalid opcode: $~x ~a"
                             op cpu))])))

  (expand-opcode-table create-emulator [#:emulate-one emulate-one-instruction])

  (struct cpu ([PC : Fixnum]
               [A : Fixnum]
               [X : Fixnum]
               [Y : Fixnum]
               [SP : Fixnum]
               [P : Fixnum]
               [read-proc : (-> Fixnum Fixnum)]
               [write-proc : (-> Fixnum Fixnum Void)])
    #:mutable #:type-name CPU)

  (: make-cpu (-> (-> Fixnum Fixnum) (-> Fixnum Fixnum Void) CPU))
  (define (make-cpu cpu-read cpu-write)
    ; TODO these startup values are specific to nestest,
    ; what should they actually be?
    (cpu #xC000 ; PC
         0 ; A
         0 ; X
         0 ; Y
         #xFD ; SP
         #x24 ; ST
         cpu-read
         cpu-write))

  (define-syntax-rule (with-cpu cpu-expr body ...)
    (let* ([cpu cpu-expr]
           [pc : Fixnum (cpu-PC cpu)]
           [a : Fixnum (cpu-A cpu)]
           [x : Fixnum (cpu-X cpu)]
           [y : Fixnum (cpu-Y cpu)]
           [sp : Fixnum (cpu-SP cpu)]
           [p : Fixnum (cpu-P cpu)]
           [read-proc (cpu-read-proc cpu)]
           [write-proc (cpu-write-proc cpu)])
      (define retval
        (syntax-parameterize
            ([cpu-read (lambda (stx)
                         (syntax-case stx ()
                           [(_ addr) #'(read-proc addr)]))]
             [cpu-write (lambda (stx)
                          (syntax-case stx ()
                            [(_ addr val) #'(write-proc addr val)]))])
          (parameterize-syntax-ids ([PC pc]
                                    [A a]
                                    [X x]
                                    [Y y]
                                    [SP sp]
                                    [P p])
                                   body ...)))
      (set-cpu-PC! cpu pc)
      (set-cpu-A! cpu a)
      (set-cpu-X! cpu x)
      (set-cpu-Y! cpu y)
      (set-cpu-SP! cpu sp)
      (set-cpu-P! cpu p)
      retval))

  (: cpu-step (-> CPU Fixnum))
  (define (cpu-step cpu)
    (with-cpu cpu
      (let* ([opcode (cpu-read PC)])
        (PC (ufx+ 1 PC))
        (let ([cycles (ann (emulate-one-instruction opcode)
                           Fixnum)])
          cycles))))

  (: cpu-reset (-> CPU Fixnum))
  (define (cpu-reset cpu)
    (with-cpu cpu
      (reset #:address -42 #:opcode -420)))

  (: cpu-nmi (-> CPU Fixnum))
  (define (cpu-nmi cpu)
    (with-cpu cpu
      (nmi #:address -42 #:opcode -420)))

  (: cpu-get-state (-> CPU (List Fixnum Fixnum Fixnum Fixnum Fixnum Fixnum)))
  (define (cpu-get-state cpu)
    (list (cpu-PC cpu)
          (cpu-A cpu)
          (cpu-X cpu)
          (cpu-Y cpu)
          (cpu-SP cpu)
          (cpu-P cpu)))

  ; end module one-big-proc
  }


(require (submod 'struct-and-procs))
;(require (submod 'one-big-proc))

(module+ test
  ; Convert number to hexadecimal string to match known good log
  (define (~h [val : Fixnum] [width : Exact-Nonnegative-Integer 2])
    (~a (string-upcase (format "~x" val))
        #:width width #:align 'right #:left-pad-string "0"))

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
  (: cpu-read (-> Fixnum Fixnum))
  (define (cpu-read addr)
    (if (addr . ufx< . #x8000)
        (bytes-ref ram addr)
        (bytes-ref nestest-bytes (ufxmodulo addr #x4000))))
  (: cpu-write (-> Fixnum Fixnum Void))
  (define (cpu-write addr value)
    (if (addr . ufx< . #x8000)
        (bytes-set! ram addr value)
        (bytes-set! nestest-bytes (ufxmodulo addr #x4000) value)))

  (define the-cpu (make-cpu cpu-read cpu-write))

  (: get-instruction-bytes (-> Fixnum (-> Fixnum Fixnum) (Listof Fixnum)))
  (define (get-instruction-bytes pc cpu-read)
    (let* ([opcode (cpu-read pc)]
           [operation (or (get-opcode-details opcode)
                          (error "unknown opcode:" opcode))]
           [bytes (operation-bytes operation)])
      (case bytes
        [(1) (list opcode)]
        [(2) (list opcode
                   (cpu-read (ufx+ pc 1)))]
        [(3) (list opcode
                   (cpu-read (ufx+ pc 1))
                   (cpu-read (ufx+ pc 2)))]
        [else (error "too many bytes:" opcode)])))

  ; Make sure the first instruction is where it should be
  (check-equal? (cpu-read #xC000) #x4C)
  (check-equal? (cpu-read #x8000) #x4C)
  ; Disable log for speed
  (define log? #f)
  ; Run until PC reaches last line of known good log
  (let loop ()
    (let* ([cpu-state (cpu-get-state the-cpu)]
           [orig-PC (first cpu-state)]
           [opcode (cpu-read orig-PC)]
           [operation (or (get-opcode-details opcode)
                          (error "unknown opcode:" opcode))]
           [_(when log?
               (display (~h orig-PC 4)))]
           [bytes (get-instruction-bytes orig-PC cpu-read)]
           [byte-string (string-join (map ~h bytes) " ")]
           [a-string (format "A:~a" (~h (second cpu-state)))]
           [x-string (format "X:~a" (~h (third cpu-state)))]
           [y-string (format "Y:~a" (~h (fourth cpu-state)))]
           [p-string (format "P:~a" (~h (sixth cpu-state)))]
           [sp-string (format "SP:~a" (~h (fifth cpu-state)))]
           [cyc-string (format "CYC:~a" cycles)])
      (let ([cyc (cpu-step the-cpu)])
        (set! cycles (ufx+ cycles cyc)))
      (define new-PC (first (cpu-get-state the-cpu)))
      (define disassembly (operation-mnemonic operation))
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
                 (not (ufx= new-PC #xC66E)))
        (loop))))
  )
