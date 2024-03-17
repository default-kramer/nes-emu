#lang typed/racket

(provide run-pixel-test)

(require (prefix-in cpu: "../core/cpu.rkt")
         (prefix-in ppu: "../core/ppu.rkt")
         "../core/emulator.rkt"
         "../ufx.rkt"
         racket/unsafe/ops
         typed/rackunit)

(: run-pixel-test (-> Path Path Path Fixnum Void))
(define (run-pixel-test rom-path actual-path expected-path to-frame)

  (define-syntax-rule (define-multi [a ...] ...)
    (begin (define a ...)
           ...))

  (define-multi
    [system-clock : Fixnum 0]
    [frame-count : Fixnum 0]
    [bg-hash : Fixnum 17]
    [cpu-cycles : Fixnum 0])

  (define-syntax-rule (display* #:port port item ...)
    (begin (display item port)
           ...))

  (: update-hash Pixel-Callback)
  (define (update-hash x y index)
    (set! bg-hash (ufxand #x3FFFFFFF (ufx+ index (ufx* bg-hash 31)))))

  (define emu (make-emulator rom-path update-hash))
  (define clock (emulator-clock emu))
  (define reset (emulator-reset emu))

  (define (bus-clock [ppulog : (U #f Output-Port) #f] [cpulog : (U #f Output-Port) #f])
    (define prev-hash bg-hash)
    (define frame-complete? (clock))
    (when frame-complete?
      (set! frame-count (ufx+ 1 frame-count)))
    (when (and ppulog frame-complete?)
      (display* #:port ppulog
                frame-count" "prev-hash"\n")))

  (let ()
    (reset)
    (let ([ppulog (open-output-file actual-path #:exists 'replace)])
      (with-handlers ([exn? (lambda (e)
                              (fail (format "exception during emulation: ~a" e)))])
        (let loop ()
          (bus-clock ppulog)
          (when (< frame-count to-frame)
            (loop))))
      (close-output-port ppulog))
    (let* ([expected (port->string (open-input-file expected-path))]
           [actual (port->string (open-input-file actual-path))])
      (if (not (equal? actual expected))
          (fail (format "files differ: ~a vs ~a" expected-path actual-path))
          (check-equal? 1 1))))
  (void))
