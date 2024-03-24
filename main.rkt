#lang racket/gui

(define rom-path
  #;"./roms/nes-roms/Donkey Kong (Japan).nes"
  "./roms/SMB.nes")

(module mod-create-place racket
  ; If this code goes into the main submodule, I get
  ;   "cannot instantiate `racket/gui/base' a second time..."
  (provide create-place)
  (require "place-emulator.rkt")
  (define (create-place)
    (place channel
           (run-place-emulator channel))))
(require (submod 'mod-create-place))

{module+ main
  (require "place-emulator.rkt"
           (only-in "core/emulator.rkt" validate-rom)
           "ufx.rkt"
           racket/unsafe/ops
           pict)

  ; Make sure the ROM is supported before starting anything else:
  (println "Checking if ROM is supported...")
  (validate-rom rom-path)
  (println "...looks good!")

  (define comms (make-shared-bytes 3))

  (define buffers
    ; Let's just try 3 buffers for fun
    (vector-immutable (make-pict-buffer)
                      (make-pict-buffer)
                      (make-pict-buffer)))

  (define the-place (create-place))
  ; Send the configuration message:
  (place-channel-put the-place (list comms
                                     (vector->list buffers)))

  (define (update-controller code release?)
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
      (let* ([value (bytes-ref comms 1)] ; Player 1 controller status
             [value (if release?
                        (ufxand value (ufxnot bit))
                        (ufxior value bit))])
        (bytes-set! comms 1 value))))

  (define running? #t)
  (define main-frame%
    (class frame%
      (super-new)
      (define/augment (on-close)
        (set! running? #f)
        (place-channel-put the-place (list 'shutdown))
        (println "sent shutdown message to place"))
      (define/override (on-subwindow-char receiver key-event)
        ; It would probably be more convenient if we could poll the
        ; keyboard state on demand instead of watching for events...
        ; but I don't know if that exists
        (let* ([code (send key-event get-key-code)]
               [release? (equal? 'release code)]
               [code (if release?
                         (send key-event get-key-release-code)
                         code)])
          (update-controller code release?)))))

  (define frame (new main-frame%
                     [label "Test"]
                     [width 600]
                     [height 600]))

  (define prev-canvas-index -1)
  (define canvas
    (new canvas% [parent frame]
         [paint-callback
          (lambda (canvas dc)
            (let* ([idx (bytes-ref comms 0)]
                   [buffer (vector-ref buffers idx)]
                   [pict (argb-pixels->pict buffer W)]
                   [pict (scale pict 2)])
              (draw-pict pict dc 0 0)
              (let ([expected (case prev-canvas-index
                                [(-1) idx] ; first frame only
                                [(0) 1]
                                [(1) 2]
                                [(2) 0])])
                (when (not (= expected idx))
                  (println (list "canvas slept through an entire frame??" prev-canvas-index idx))))
              (set! prev-canvas-index idx)))]))

  (send frame show #t)
  (place-channel-put the-place (list 'launch-rom rom-path))
  (let ([prev-buffer-index -1])
    (let loop ()
      (sleep/yield 0)
      (let ([new-buffer-index (unsafe-bytes-ref comms 0)])
        (when (not (= prev-buffer-index new-buffer-index))
          (set! prev-buffer-index new-buffer-index)
          (send canvas refresh-now))
        (when running?
          (loop)))))
  }
