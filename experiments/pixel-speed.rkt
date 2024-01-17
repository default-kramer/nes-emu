#lang racket/gui

; Use make-shared-bytes to set up the PPU rendering buffer.
; Set up a "PPU simulator" running in a separate place, which continuously
; rewrites the shared bytes as fast as it possibly can.
; Then on the GUI thread, demonstrate that we can do about 200FPS of:
; * argb-pixels->pict (using the shared bytes)
; * scale that pict by 2
; * render it onto a canvas (and the screen)
; So with this approach, the GUI thread appears to be more than adequate.
;
; The "PPU simulator" also looks promising, as it can rewrite all the shared bytes
; pretty quickly. The blue half of the cycle occurs slightly more than once
; per second, meaning it can rewrite the entire buffer slightly more than
; (* 255 10) times per second. Of course, the calculations needed to actually
; emulate a PPU will be more costly than simply incrementing a value, but
; since there are no layers of abstraction (like the GUI) involved, I am
; confident that Racket will have no problems here.
;
; Finally, these numbers were captured from within DrRacket.
; As I recall, they should be even better after using `raco exe`.

(require pict)

(module TRY2 racket
  (provide start-worker)
  (require pict)

  (module TYPED typed/racket
    (provide run-loop)
    (require racket/unsafe/ops)
    (define W 256)
    (define H 240)
    (define + unsafe-fx+)
    (define mod unsafe-fxmodulo)
    (define bytes-set! unsafe-bytes-set!)

    (: randomize (-> Bytes Byte Byte Byte Void))
    (define (randomize bytes r g b)
      (define i : Fixnum 0)
      (for ([ignored (in-range (* W H))])
        (bytes-set! bytes i 255) ; A
        (set! i (+ 1 i))
        (bytes-set! bytes i r) ; R
        (set! i (+ 1 i))
        (bytes-set! bytes i g) ; G
        (set! i (+ 1 i))
        (bytes-set! bytes i b) ; B
        (set! i (+ 1 i))
        ))

    (: run-loop (-> Bytes Void))
    (define (run-loop bytes)
      (define i : Fixnum 0)
      (define-syntax-rule (increment color)
        (begin (set! color (mod (+ 1 color) 255))
               (when (unsafe-fx= 0 color)
                 (set! i (mod (+ 1 i) 10)))))
      (let* ([r : Byte 0]
             [g : Byte 0]
             [b : Byte 0])
        (let loop ()
          (case i
            [(0) (increment r)]
            [(1 2 3 4) (increment g)]
            [(5 6 7 8 9) (increment b)])
          (randomize bytes r g b)
          (loop)))))
  (require (submod 'TYPED))

  (define (start-worker)
    (place channel
           (let ([bytes (place-channel-get channel)])
             (println "worker started")
             (run-loop bytes))))
  )

(require (submod 'TRY2))
(define W 256)
(define H 240)


(module+ main
  (define start-millis #f)
  (define frame-count 0)

  (define the-place (start-worker))
  (define the-bytes (make-shared-bytes (* W H 4) 50))
  (place-channel-put the-place the-bytes)
  (define (repaint)
    (argb-pixels->pict the-bytes W))

  (define frame (new frame% [label "Test"]
                     [width 600]
                     [height 600]))

  (define canvas (new canvas% [parent frame]
                      [paint-callback
                       (lambda (canvas dc)
                         (when (not start-millis)
                           (set! start-millis (current-inexact-milliseconds)))
                         (when (= frame-count 60)
                           (let* ([elapsed-millis (- (current-inexact-milliseconds)
                                                     start-millis)]
                                  [fps (* 1000 (/ frame-count elapsed-millis))])
                             (println (format "FPS ~a" fps))
                             (set! frame-count 0)
                             (set! start-millis (current-inexact-milliseconds))))
                         (set! frame-count (add1 frame-count))
                         (let* ([pict (repaint)]
                                [pict (scale pict 2)])
                           (draw-pict pict dc 0 0)))]))

  (send frame show #t)
  (let loop ()
    (send canvas refresh-now)
    (sleep/yield 0)
    (loop))
  )
