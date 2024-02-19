#lang typed/racket

(require "cpu.rkt"
         "nestest.rkt"
         "ufx.rkt")

; Set up memory like this:
;   0000 - 7FFF general purpose RAM
;   8000 - BFFF nestest.nes
;   C000 - FFFF nestest.nes again
; I'll make the whole range writeable, that's probably not accurate...
(define nestest-bytes (create-nestest-bytes))
(define ram (make-bytes #x8000))
(: .read (-> Fixnum Fixnum))
(define (.read addr)
  (if (addr . ufx< . #x8000)
      (bytes-ref ram addr)
      (bytes-ref nestest-bytes (ufxmodulo addr #x4000))))
(: .write (-> Fixnum Fixnum Void))
(define (.write addr value)
  (if (addr . ufx< . #x8000)
      (bytes-set! ram addr value)
      (bytes-set! nestest-bytes (ufxmodulo addr #x4000) value)))

; Wire up syntax parameters and emulate
(define (run-test)
  (define cpu (make-cpu .read .write))
  (define cycles : Fixnum 7)
  ; Run until PC reaches last line of known good log
  (let loop ()
    (let ([cyc (cpu-step cpu)])
      (set! cycles (ufx+ cycles cyc)))
    ; TODO we really want the PC, using get-state is awkward:
    (when (not (ufx= (first (cpu-get-state cpu)) #xC66E))
      (loop)))
  cycles)

(let ([start (current-inexact-milliseconds)]
      [runs 3000])
  (for ([i (in-range runs)])
    (let ([cycles (run-test)])
      (when (not (ufx= cycles 26554))
        (error "unexpected cycles:" cycles))))
  (let ([end (current-inexact-milliseconds)])
    (println (list "runs:" runs "millis:" (- end start)))
    (void)))
