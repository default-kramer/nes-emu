#lang typed/racket

(provide run-place-emulator W
         pict-buffer? make-pict-buffer)

(require "ufx.rkt"
         "core/emulator.rkt"
         "pal-screen.rkt"
         racket/flonum
         racket/unsafe/ops)

(define W 256)
(define H 240)

; Each pixel needs 4 bytes (ARGB)
(define pict-buffer-size (* W H 4))

(define (pict-buffer? x)
  (and (bytes? x)
       (= pict-buffer-size (bytes-length x))))

(define (make-pict-buffer)
  (make-shared-bytes pict-buffer-size 50))

; Comms is a byte array
; 0 : Index of most recently updated pict-buffer (place writes)
; 1 : First player controller status (GUI writes)
; 2 : Second player controller status (GUI writes)
(define (comms? x)
  (and (bytes? x)
       (= 3 (bytes-length x))))

(: run-place-emulator (-> Place-Channel Void))
{define (run-place-emulator channel)
  (println "Place emulator awaiting configuration message")

  (: comms Bytes)
  (: buffers (Immutable-Vectorof Bytes))
  (define-values (comms buffers)
    (match (place-channel-get channel) ; block for configuration message
      [(list comms (list buffers ...))
       #:when (and (bytes? comms)
                   (comms? comms)
                   (not (empty? buffers))
                   (andmap bytes? buffers)
                   (andmap pict-buffer? buffers))
       (values comms (apply vector-immutable buffers))]
      [else
       (error "Invalid configuration message; quitting emulation")]))
  (println "Configuration message received")

  (define current-pict-index : Byte 0)
  ; Mutable but always holds a `pict-buffer?`
  (define current-pict-buffer : Bytes
    (vector-ref buffers current-pict-index))


  (: sample-controller Sample-Controller)
  (define (sample-controller i)
    ; Assert: i is 0 or 1
    ; Controller state is in bytes 1 and 2 of comms:
    (unsafe-bytes-ref comms (ufx+ 1 (ufxand 1 i))))

  (: pixel-callback Pixel-Callback)
  (define (pixel-callback x y index)
    (let ([pixel-index (ufx* 4 (ufx+ x (ufx* y W)))])
      (when (and (ufx>= pixel-index 0)
                 (ufx< pixel-index pict-buffer-size))
        (let* ([current-pixel (unsafe-vector-ref pal-screen index)]
               [r (unsafe-vector-ref current-pixel 0)]
               [g (unsafe-vector-ref current-pixel 1)]
               [b (unsafe-vector-ref current-pixel 2)]
               [buffer current-pict-buffer])
          (unsafe-bytes-set! buffer pixel-index #xFF) ; A
          (set! pixel-index (ufx+ 1 pixel-index))
          (unsafe-bytes-set! buffer pixel-index r)
          (set! pixel-index (ufx+ 1 pixel-index))
          (unsafe-bytes-set! buffer pixel-index g)
          (set! pixel-index (ufx+ 1 pixel-index))
          (unsafe-bytes-set! buffer pixel-index b)))))

  (define-type State (U (List 'stop) ; no emulator active, waiting
                        (List 'run Emulator)
                        (List 'shutdown) ; exit, stop listening
                        ))

  (: parse-TODO (-> Any (U #f State)))
  ; Parses only those messages which cause a State transition, I think??
  (define (parse-TODO msg)
    (match msg
      [(list 'launch-rom path)
       #:when (string? path)
       (let ([emu (make-emulator path
                                 pixel-callback
                                 sample-controller)])
         (ann (list 'run emu) State))]
      [(list 'stop)
       (ann (list 'stop) State)]
      [(list 'shutdown)
       (ann (list 'shutdown) State)]
      [else #f]))

  (: run-emulation-loop (-> Emulator State))
  (define (run-emulation-loop emu)
    (define reset (emulator-reset emu))
    (define clock (emulator-clock emu))
    (reset)

    (: handle-message (-> Any Void))
    (define (handle-message msg)
      ; Future messages (eg pause/unpause) probably belong here.
      (error "Unrecognized message" msg))

    (: read-messages (-> (U #f State)))
    (define (read-messages)
      (let ([msg (sync/timeout 0 channel)])
        (or (parse-TODO msg)
            (and msg (begin (handle-message msg)
                            (read-messages)))
            #f)))

    (define anchor-time : Flonum (current-inexact-milliseconds))
    (define frames-since-anchor : Fixnum 0)
    (define millis-per-frame : Flonum (/ 1000.0 60))
    (define frame-due : Flonum anchor-time)

    (: maybe-sleep (-> Void))
    (define (maybe-sleep)
      (let* ([now (current-inexact-milliseconds)]
             [sleep-millis (fl- frame-due now)])
        #;(when (fl< sleep-millis 0.0)
            (println (list "LATE FRAME" sleep-millis frame-due)))
        (if (fl< sleep-millis 15.0)
            (void)
            (begin (sleep (fl/ sleep-millis 1000.0))
                   (void)))))

    (: do-one-frame (-> Void))
    (define (do-one-frame)
      (define frame-complete? (clock))
      (if frame-complete?
          (begin
            ; comms[0] indicates the most recent pict buffer
            (unsafe-bytes-set! comms 0 current-pict-index)
            (let* ([next (ufx+ 1 current-pict-index)]
                   [next (ufxmodulo next (vector-length buffers))]
                   [next : Byte (ufxand 255 next)])
              (set! current-pict-index next)
              (set! current-pict-buffer (vector-ref buffers next)))
            (set! frame-due (fl+ frame-due millis-per-frame))
            (set! frames-since-anchor (ufx+ 1 frames-since-anchor))
            (let ([sample-freq   120]
                  [dividend (fl* 120.0 1000.0)])
              (when (ufx= frames-since-anchor sample-freq)
                (let* ([now (current-inexact-milliseconds)]
                       [elapsed-millis (fl- now anchor-time)]
                       [fps (fl/ dividend elapsed-millis)])
                  (println (format "EMU FPS: ~a" (~r fps #:precision 2))))
                ; Re-anchor:
                (set! anchor-time (fl- frame-due millis-per-frame))
                (set! frames-since-anchor 0))))
          (do-one-frame)))

    (let loop ()
      (let ([state (read-messages)])
        (cond
          [state state]
          [else (begin
                  (maybe-sleep)
                  (do-one-frame)
                  (loop))]))))

  (: run-state-loop (-> State 'TERMINATED))
  (define (run-state-loop [state : State])
    (match state
      [(list 'shutdown)
       'TERMINATED]
      [(list 'run emu)
       (run-state-loop (run-emulation-loop emu))]
      [(list 'stop)
       (let* ([msg (place-channel-get channel)] ; block
              [todo (parse-TODO msg)])
         (when (not todo)
           (error "Invalid message while emulator is not running:" msg))
         (run-state-loop todo))]))
  (run-state-loop (list 'stop))
  (println "Terminating place emulator")
  }
