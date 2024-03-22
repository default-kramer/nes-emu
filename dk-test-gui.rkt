#lang racket/gui

(require pict)

(module WTF racket
  (provide start-worker W H)

  (module TYPED typed/racket
    (provide run-loop W H)

    (require "dk-test.rkt" "ufx.rkt"
             racket/unsafe/ops)

    (define W 256)
    (define H 240)

    (: run-loop (-> Place-Channel Void))
    (define (run-loop channel)
      (println "Emulation place is running!")
      (bus-reset)
      (println "Reset emulator")
      (let* ([frame-count : Fixnum 0]
             [last-time : Flonum (current-inexact-milliseconds)]
             #;[item (place-channel-get channel)]
             #;[buffer : Bytes (or (and (bytes? item) item)
                                   (error "Didn't get bytes buffer"))])

        (: do-one-frame (-> Bytes Void))
        (define (do-one-frame buffer)
          (define frame-complete? (bus-clock))
          (let ([pixel-index : Fixnum
                             (ufx* 4 (ufx+ (ufx* current-pixel-y W)
                                           current-pixel-x))])
            (when (and (ufx>= pixel-index 0)
                       (ufx< pixel-index (bytes-length buffer)))
              (let ([r (unsafe-vector-ref current-pixel 0)]
                    [g (unsafe-vector-ref current-pixel 1)]
                    [b (unsafe-vector-ref current-pixel 2)])
                (unsafe-bytes-set! buffer pixel-index #xFF) ; A
                (set! pixel-index (ufx+ 1 pixel-index))
                (unsafe-bytes-set! buffer pixel-index r)
                (set! pixel-index (ufx+ 1 pixel-index))
                (unsafe-bytes-set! buffer pixel-index g)
                (set! pixel-index (ufx+ 1 pixel-index))
                (unsafe-bytes-set! buffer pixel-index b))))
          (if frame-complete?
              (void)
              (do-one-frame buffer)))

        (define (update-controller [code : Any] [release? : Boolean])
          (define bit (case code
                        [(right) 1]
                        [(left)  2]
                        [(down)  4]
                        [(up)    8]
                        [(#\r #\R) #x10] ; Start
                        [(#\e #\E) #x20] ; Select
                        [(#\d #\D) #x40] ; B
                        [(#\f #\F) #x80] ; A
                        [else #f]))
          (when bit
            (set-controller-0 bit release?)))

        (let loop ()
          (let ([item (place-channel-get channel)])
            (match item
              [(list 'press keycode)
               (update-controller keycode #f)]
              [(list 'release keycode)
               (update-controller keycode #t)]
              [x #:when (bytes? item)
                 (begin #;(set! buffer item)
                        (do-one-frame item)
                        (place-channel-put channel 42)
                        (set! frame-count (ufx+ 1 frame-count))
                        (when (ufx= 60 frame-count)
                          (let* ([now (current-inexact-milliseconds)]
                                 [elapsed (unsafe-fl- now last-time)]
                                 [millis-per-frame (unsafe-fl/ elapsed 60.0)]
                                 [fps (unsafe-fl/ 1000.0 millis-per-frame)])
                            (set! last-time now)
                            (set! frame-count 0)
                            (println (list "Emu FPS:" fps)))))]
              [else
               (error "Bad channel message?" item)]))
          (loop)))))

  (require (submod 'TYPED))
  
  (define (start-worker)
    (place channel
           (let (#;[bytes (place-channel-get channel)])
             (println "worker started")
             (run-loop channel)))))

(require (submod 'WTF))

(module+ main
  (define buffer1 (make-shared-bytes (* W H 4) 50))
  (define buffer2 (make-shared-bytes (* W H 4) 50))
  #;(define (start-worker)
      (place channel
             (let (#;[bytes (place-channel-get channel)])
               (println "worker started")
               (run-loop channel))))

  (define start-millis #f)
  (define frame-count 0)

  (define the-place (start-worker))
  (define gui-buffer buffer1)
  (define worker-buffer buffer2)
  (define (request-new-frame)
    (place-channel-put the-place worker-buffer))
  (request-new-frame)
  (define (repaint)
    (let ([pict (argb-pixels->pict gui-buffer W)]
          [old-gui-buffer gui-buffer]
          [old-worker-buffer worker-buffer]
          [response (sync/timeout 0 the-place)])
      (when response
        (set! gui-buffer old-worker-buffer)
        (set! worker-buffer old-gui-buffer)
        (request-new-frame))
      #;(let ([item (place-channel-get the-place)])
          (println (list "GOT BACK" item)))
      pict))

  (define main-frame%
    (class frame%
      (super-new)
      (define/override (on-subwindow-char receiver key-event)
        ; It would probably be more convenient if we could poll the
        ; keyboard state on demand instead of watching for events...
        ; but I don't know if that exists
        (let* ([code (send key-event get-key-code)]
               [release? (equal? 'release code)]
               [message (if release?
                            (list 'release (send key-event get-key-release-code))
                            (list 'press code))])
          (place-channel-put the-place message)))))

  (define frame (new main-frame%
                     [label "Test"]
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
                             (println (format "canvas FPS ~a" fps))
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