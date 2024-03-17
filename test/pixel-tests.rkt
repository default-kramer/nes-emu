#lang typed/racket

{module+ test
  (require "pixel-test-helper.rkt"
           typed/rackunit)

  (define roms-dir (build-path (current-directory) 'up "roms" "nes-roms"))
  (define logs-dir (build-path (current-directory) "pixel-test-logs"))

  (define (test [rom-name : String] [to-frame : Fixnum])
    (let ([rom-path (build-path roms-dir rom-name)]
          [actual-path (build-path logs-dir (format "~a.actual.pixelhash.log" rom-name))]
          [expect-path (build-path logs-dir (format "~a.expect.pixelhash.log" rom-name))])
      (when (not (file-exists? expect-path))
        (with-output-to-file expect-path
          (lambda () (println "no expectation configured"))))
      (run-pixel-test rom-path actual-path expect-path to-frame)))

  (test "Donkey Kong (Japan).nes" 2628)

  ; Unvetted against another implementation:
  (test "Bomberman (USA).nes" 2500) ; Mapper 0, Vertical
  (test "Dig Dug (Japan).nes" 2500) ; Mapper 0, Horizontal
  (test "Mario Bros. (Europe) (Rev A).nes" 2500) ; Mapper 0, Horizontal
  (test "Mario Bros. (World).nes" 2500) ; Mapper 0, Vertical
  (test "Galaga (Japan).nes" 2500) ; Mapper 3 according to header, but is this true?
  }
