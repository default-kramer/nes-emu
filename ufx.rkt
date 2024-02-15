#lang typed/racket

(provide (all-from-out (submod 'builtin))
         ufxior*)

; Rename and provide "unsafe-fx" procedures with a "ufx" naming convention.
(module builtin typed/racket
  (require racket/fixnum
           racket/unsafe/ops)

  (require typed/racket/unsafe)
  #;(unsafe-require/typed
     racket/unsafe/ops
     [(unsafe-fx+/wraparound ufx+) (-> Fixnum Fixnum Fixnum)]
     [(unsafe-fx-/wraparound ufx-) (-> Fixnum Fixnum Fixnum)]
     [(unsafe-fx*/wraparound ufx*) (-> Fixnum Fixnum Fixnum)])
  #;(provide ufx+ ufx- ufx*)

  (provide (rename-out [unsafe-fx+ ufx+]
                       [unsafe-fx- ufx-]
                       [unsafe-fx* ufx*]
                       [unsafe-fxnot ufxnot]
                       [unsafe-fxand ufxand]
                       [unsafe-fxxor ufxxor]
                       [unsafe-fxior ufxior]
                       [unsafe-fxlshift ufxlshift]
                       [unsafe-fxrshift ufxrshift]
                       [unsafe-fxmodulo ufxmodulo]
                       ; The following procedures have no unsafe variant,
                       ; but use the "ufx" name anyway so I don't have to remember.
                       [fx=  ufx= ]
                       [fx>= ufx>=]
                       [fx>  ufx> ]
                       [fx<= ufx<=]
                       [fx<  ufx< ]
                       ))
  )

(require (submod 'builtin))

(define-syntax (ufxior* stx)
  (syntax-case stx ()
    [(_ a)
     (syntax/loc stx a)]
    [(_ a b)
     (syntax/loc stx
       (ufxior a b))]
    [(_ a b c ...)
     (syntax/loc stx
       (ufxior a (ufxior* b c ...)))]))
