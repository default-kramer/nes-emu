#lang typed/racket

(provide bus-reset bus-clock
         set-controller-0
         current-pixel current-pixel-x current-pixel-y)

(require "core/emulator.rkt"
         "ufx.rkt"
         "pal-screen.rkt"
         racket/unsafe/ops)

(define current-pixel : RGB #(0 0 0))
(define current-pixel-x : Fixnum 0)
(define current-pixel-y : Fixnum 0)

(: pixel-callback Pixel-Callback)
(define (pixel-callback x y index)
  (set! current-pixel (unsafe-vector-ref pal-screen index))
  (set! current-pixel-x x)
  (set! current-pixel-y y))

(define controller-0 : Fixnum 0)
(define controller-1 : Fixnum 0)

(: sample-controller Sample-Controller)
(define (sample-controller index)
  (case index
    [(0) controller-0]
    [(1) controller-1]))

(define (set-controller-0 [bit : Byte] [released? : Boolean])
  (if released?
      (set! controller-0 (ufxand (ufxnot bit) controller-0))
      (set! controller-0 (ufxior bit controller-0))))

(define emu (make-emulator
             ;"./roms/nes-roms/Donkey Kong (Japan).nes"
             ;"./roms/nes-roms/Bomberman (USA).nes"
             ;"./roms/nes-roms/Mario Bros. (World).nes"
             "./roms/SMB.nes"
             pixel-callback
             sample-controller))

(: bus-reset (-> Void))
(define bus-reset (emulator-reset emu))

(: bus-clock (-> Boolean))
(define bus-clock (emulator-clock emu))
