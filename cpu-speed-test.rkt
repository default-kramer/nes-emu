#lang typed/racket

(require "cpu.rkt"
         "nestest.rkt"
         "doodling.rkt"
         "util.rkt"
         "ufx.rkt"
         racket/require
         (filtered-in
          (λ (name) ; Whenever I say `fx+` I actually mean `unsafe-fx+`
            (and (regexp-match #rx"^ufx" name)
                 (regexp-replace #rx"ufx" name "fx")))
          "ufx.rkt")
         racket/stxparam)

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
(define (run-test)
  (define .PC : Fixnum #xC000)
  (define .A : Fixnum 0)
  (define .X : Fixnum 0)
  (define .Y : Fixnum 0)
  (define .SP : Fixnum #xFD)
  (define .ST : Fixnum #x24)
  (define cycles : Fixnum 7)
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
     ; Make sure these compile
     (define (proc-step) (step))
     (define (proc-reset) (reset))
     (define (proc-nmi) (nmi))
     ; Run until PC reaches last line of known good log
     (let loop ()
       (let ([cyc (proc-step)])
         (set! cycles (fx+ cycles cyc)))
       (when (not (fx= .PC #xC66E))
         (loop)))
     cycles)))

(let ([start (current-inexact-milliseconds)]
      [runs 300])
  (for ([i (in-range runs)])
    (let ([cycles (run-test)])
      (when (not (ufx= cycles 26554))
        (error "unexpected cycles:" cycles))))
  (let ([end (current-inexact-milliseconds)])
    (println (list "runs:" runs "millis:" (- end start)))
    (void)))
