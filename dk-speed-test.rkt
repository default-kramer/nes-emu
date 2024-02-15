#lang racket

(require profile)

(module TYPED racket
  (provide do-some-frames)
  (require "dk-test.rkt")

  (bus-reset)

  ;(: do-one-frame (-> Void))
  (define (do-one-frame)
    (let loop ()
      (bus-clock)
      (when (not (TODO))
        (loop))))

  (for ([i (in-range 10)])
    (do-one-frame))

  (define (do-some-frames)
    (for ([i (in-range 600)])
      (do-one-frame))))

(require (submod 'TYPED))

(let* ([start (current-inexact-milliseconds)]
       [_ (do-some-frames)]
       [end (current-inexact-milliseconds)]
       [millis (- end start)]
       [frames 600]
       [fps (/ frames (/ millis 1000))])
  (println (format "Did ~a frames in ~ams, ~a FPS" frames millis fps)))
