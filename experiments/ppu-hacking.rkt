#lang racket/gui

(require racket/fixnum
         racket/draw
         pict
         sgl/gl
         sgl/gl-vectors
         sgl/bitmap
         "../nestest.rkt")

(define bytes
  (parameterize ([current-directory ".."])
    (create-nestest-bytes)))

(define (ppu-read addr)
  (bytes-ref bytes addr))

(define colors (list (make-color 0 0 0)
                     (make-color 255 0 0)
                     (make-color 0 255 0)
                     (make-color 255 255 255)))

(define pens (map (lambda (color) (new pen% [color color]))
                  colors))

(define (generate-stuff buffer)
  (define bytes-index 7)
  (for ([y (in-range 16)])
    (for ([x (in-range 16)])
      (define nOffset (+ (* y 256) (* x 16)))
      (for ([row (in-range 8)])
        (for ([col (in-range 8)])
          (let* ([pixel (fxand (fxmodulo x 4) (fxmodulo y 4))])
            (bytes-set! buffer (- bytes-index col) pixel)
            ))
        (set! bytes-index (+ 8 bytes-index))
        ))))

; Don't construct the image. Instead, fill the buffer with pixel values of [0-3]
(define (try-render-tile i buffer)
  ; Each tile is 8x8 pixels, and there are 16x16 tiles
  #;(define bitmap (make-object bitmap% 128 128))
  #;(define dc (make-object bitmap-dc% bitmap))
  (define bytes-index 7)
  (for ([y (in-range 16)])
    (for ([x (in-range 16)])
      (define nOffset (+ (* y 256) (* x 16)))
      (for ([row (in-range 8)])
        (let* ([tile_lsb (ppu-read (+ (* i #x1000) nOffset row 0))]
               [tile_msb (ppu-read (+ (* i #x1000) nOffset row 8))])
          ; Shift left here so that the bit we want will be where we want it
          (set! tile_msb (fxlshift tile_msb 1))
          (for ([col (in-range 8)])
            (let* ([pixel (fxior (fxand tile_lsb 1)
                                 (fxand tile_msb 2))])
              (set! tile_lsb (fxrshift tile_lsb 1))
              (set! tile_msb (fxrshift tile_msb 1))
              #;(send dc set-pixel
                      (+ (* x 8) (- 7 col)); row)
                      (+ (* y 8) row)
                      (list-ref colors pixel))
              (bytes-set! buffer (- bytes-index col) pixel)
              )))
        (set! bytes-index (+ 8 bytes-index))
        )))
  #;bitmap
  (void)
  )

(define (buffer->picts buffer)
  (define index 0)
  (for/vector ([y (in-range 16)])
    (for/vector ([x (in-range 16)])
      (define bmp (make-object bitmap% 8 8))
      (define dc (make-object bitmap-dc% bmp))
      (for ([row (in-range 8)])
        (for ([col (in-range 8)])
          (let* ([pixel (bytes-ref buffer index)])
            (set! index (add1 index))
            (send dc set-pixel col row
                  (list-ref colors pixel)))))
      (bitmap bmp))))

(define (buffer->dc buffer dc)
  (define index 0)
  (for ([y (in-range 16)])
    (for ([x (in-range 16)])
      (for ([row (in-range 8)])
        (for ([col (in-range 8)])
          (let* ([pixel (bytes-ref buffer index)])
            (set! index (add1 index))
            ; This drops to 3.7 FPS when drawing directly to the canvas DC!
            ; Surprisingly, drawing to a bitmap DC, then converting that to a pict,
            ; and then drawing that pict to the canvas DC performs slightly better at 5.2 FPS.
            ; But either way, looks like draw-point is way too slow.
            #;(begin (send dc set-pen (list-ref pens pixel))
                     (send dc draw-point
                           (fxior (fxlshift x 3) col)
                           (fxior (fxlshift y 3) row)))
            ; This does about 17 FPS when drawing to a bitmap DC, converting to pict,
            ; scaling by 2.0, and then drawing that pict onto the canvas.
            ; Looks like omitting the scaling doesn't really change anything, still 17 FPS.
            (send dc set-pixel
                  (fxior (fxlshift x 3) col)
                  (fxior (fxlshift y 3) row)
                  (list-ref colors pixel)))))))
  (void))

(define TEST-bitmap (make-object bitmap% 128 128))
(define TEST-dc (make-object bitmap-dc% TEST-bitmap))

(define (buffer->bitmap buffer)
  (buffer->dc buffer TEST-dc)
  TEST-bitmap)

(define buffer (make-bytes (* 8 8 16 16)))
(try-render-tile 4 buffer)
(define raw (buffer->bitmap buffer))
raw
(scale (bitmap raw) 2)

(define PICTS (buffer->picts buffer))

(define (random-pict!)
  (vector-ref (vector-ref PICTS (random 16))
              (random 16)))

(define frame (new frame% [label "Test"]
                   [width 600]
                   [height 600]))

(define latch #f)

(define frame-count 0)
(define start-time #f)
(define gl-context #f)

(define (TEST-GL)
  #;(begin (glBegin GL_TRIANGLES)
           (glVertex3i 1 2 3)
           (glVertex4fv (gl-float-vector 1 2 3 4))
           (glEnd))
  (begin (glClear GL_COLOR_BUFFER_BIT)
         (glRotatef 1 0 0 1)
         (glBegin GL_TRIANGLES)
         (glColor3f 1 0 0)
         (glVertex2f 0 32); /* Bottom      */
         (glColor3f 0 1 0)
         (glVertex2f -32 0 ); /* Upper Left  */
         (glColor3f 0 0 1)
         (glVertex2f 32 0 ); /* Upper Right */
         (glEnd))
  )

(define canvas (new canvas% [parent frame]
                    ;[style '(gl no-autoclear)]
                    [paint-callback
                     (lambda (canvas dc)
                       (if start-time
                           (begin (set! frame-count (add1 frame-count))
                                  (when (= 0 (fxand frame-count #xFF))
                                    (let* ([total-millis (- (current-inexact-milliseconds)
                                                            start-time)]
                                           [total-seconds (/ total-millis 1000)]
                                           [fps (/ frame-count total-seconds)])
                                      (println (format "FPS ~a" fps)))))
                           (begin (set! start-time (current-inexact-milliseconds))
                                  (set! gl-context (send dc get-gl-context))
                                  (println (list "gl-context" gl-context))))
                       (begin
                         #;(if latch
                               (try-render-tile 4 buffer)
                               (generate-stuff buffer))
                         #;(set! latch (not latch))
                         #;(let* ([bmp (buffer->bitmap buffer)]
                                  [pict (bitmap bmp)]
                                  #;[pict (scale pict 2)])
                             (draw-pict pict dc 0 0)
                             #;(buffer->dc buffer dc)
                             #;(bitmap->gl-list bmp)
                             ;(send canvas with-gl-context TEST-GL)
                             (void)
                             ))
                       #;(begin
                           (for ([x (in-range 32)])
                             (for ([y (in-range 32)])
                               (let ([pict (random-pict!)])
                                 (draw-pict pict dc (* x 8) (* y 8))))))
                       ; Turns out using the *-append procs is faster than calling
                       ; draw-pict 32x32 times!
                       (begin
                         (define (make-thing append-proc make-pict)
                           (apply append-proc (build-list 32 (lambda (x) (make-pict)))))
                         (define (make-row!) (make-thing hc-append random-pict!))
                         (define (make-col!) (make-thing vc-append make-row!))
                         (let* ([pict (make-col!)]
                                [pict (scale pict 2)]
                                [_ 1])
                           (draw-pict pict dc 0 0)))
                       #;(begin
                           (set! latch (not latch))
                           (if latch
                               (send dc draw-rectangle 10 10 60 40)
                               (send dc draw-rectangle 10 10 40 60)))
                       )]))

(module+ main
  (send frame show #t)

  ;(define canvas-dc (send canvas get-dc))
  ;(define glctx (send canvas-dc get-gl-context))
  ;(println (list (send glctx ok?) canvas-dc glctx))

  (let loop ()
    (send canvas refresh-now)
    #;(begin (send glctx call-as-current TEST-GL)
             (send glctx swap-buffers))
    (sleep/yield 0); (/ 1 120))
    (loop))
  )
