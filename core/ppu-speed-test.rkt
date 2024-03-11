#lang typed/racket

(module shared typed/racket
  (provide wish .ppu-write .ppu-read)
  (require "../ufx.rkt")
  (define randomizer : Fixnum (random 9999))
  (define (.ppu-write [addr : Fixnum] [value : Byte])
    (void))
  (: .ppu-read (-> Fixnum Byte))
  (define (.ppu-read addr)
    ; Do just enough to prevent unrealistic optimizations and/or branch predictions
    (set! randomizer (ufx+ randomizer (ufxxor randomizer addr)))
    (ufxand 255 randomizer))
  (define pixel : Fixnum 0)
  (define (compose-pixel [a : Fixnum] [b : Fixnum])
    ; Not accurate, but shouldn't matter too much:
    (set! pixel (ufxand a b)))
  (define-syntax (wish stx)
    (syntax-case stx ()
      [(_ #:ppu-read addr ppumask)
       (syntax/loc stx (.ppu-read addr))]
      [(_ #:ppu-write addr val)
       (syntax/loc stx (.ppu-write addr val))]
      [(_ #:compose-pixel a b)
       (syntax/loc stx (compose-pixel a b))])))

(module dynamic-ppu typed/racket
  (provide create-ppu)
  (require "ppu.rkt"
           (submod ".." shared))
  (define (create-ppu)
    (let ()
      (compile-ppu #:wish wish))))

(module static-ppu typed/racket
  (provide clock reset)
  (require "ppu.rkt"
           (submod ".." shared))
  (define-values (clock reset get-state register-read register-write dma-write)
    (compile-ppu #:wish wish)))

(require (prefix-in dynamic: (submod 'dynamic-ppu))
         (prefix-in static:  (submod 'static-ppu)))

(module+ test
  ; Each frame is 340 cycles * 260 scanlines.
  (begin
    (println "Static: 600 frames:")
    (static:reset)
    (time (for ([i (in-range (* 340 260 600))])
            (static:clock))))
  (begin
    (println "Dynamic: 600 frames:")
    (let-values ([(clock reset get-state register-read register-write dma-write)
                  (dynamic:create-ppu)])
      (reset)
      (time (for ([i (in-range (* 340 260 600))])
              (clock))))))
