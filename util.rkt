#lang typed/racket

(provide define-stxparams
         SET!
         parameterize-syntax-ids
         define-maskers
         has-any-flag?
         copy-bits
         )

(require racket/stxparam
         "ufx.rkt"
         (for-syntax racket/fixnum))

(define-syntax-rule (define-stxparams #:errmsg msg id ...)
  (begin
    (define-syntax-parameter id
      (lambda (stx)
        (raise-syntax-error 'id msg)))
    ...))

(define-syntax-rule (SET! id val) (id val))

(define-syntax (parameterize-syntax-ids stx)
  (syntax-case stx ()
    [(_ ([a b] ...) body ...)
     (with-syntax ([ooo (quote-syntax ...)])
       #'(syntax-parameterize ([a (lambda (stx)
                                    (syntax-case stx ()
                                      [(id val)
                                       #'(set! b val)]
                                      [id
                                       (identifier? #'id)
                                       #'b]
                                      [else
                                       #`(raise-syntax-error #f "Bad use?")]
                                      ))]
                               ...)
           body ...))]))


; Mask utils
(define-for-syntax (shift-amount x [accum 0])
  (if (fx= 1 (fxand 1 x))
      accum
      (shift-amount (fxrshift x 1) (+ 1 accum))))

(define-syntax (define-masker outerstx)
  (syntax-case outerstx ()
    [(_ id bits)
     (let* ([mask (fx+ 0 (syntax-e #'bits))]
            [antimask (fxxor mask #xFFFF)]
            [shift-count (shift-amount mask)])
       #`(define-syntax (id stx)
           (syntax-case stx ()
             [(_ #:debug)
              #'(list #,mask #,antimask #,shift-count)]
             [(_ #:mask)
              #'#,mask]
             [(_ backing #:unshifted)
              #'(ufxand #,mask backing)]
             [(_ backing #:shifted)
              #'(ufxrshift (ufxand #,mask backing) #,shift-count)]
             [(_ backing #:set! val)
              #'(SET! backing (ufxior (ufxand backing #,antimask)
                                      (ufxand #,mask (ufxlshift val #,shift-count))))]
             #;[(_ backing #:with val)
                #'(ufxior (ufxand backing #,antimask)
                          (ufxand #,mask (ufxlshift val #,shift-count)))]
             )))]))

(define-syntax-rule (define-maskers [bits id] ...)
  (begin (define-masker id bits)
         ...))

(define-syntax-rule (has-any-flag? id /flag? ...)
  (not (ufx= 0 (ufxand id (ufxior* (/flag? id #:unshifted)
                                   ...)))))

(define-syntax-rule (copy-bits #:from src #:to dst /mask ...)
  (let* ([mask (ufxior* (/mask #:mask)
                        ...)]
         ; clear relevant bits:
         [temp (ufxand dst (ufxnot mask))]
         ; copy bits
         [temp (ufxior temp (ufxand src mask))])
    (SET! dst temp)))

(module+ test
  (require typed/rackunit)
  (define-masker /bit2 #b100)
  (define-masker /bit1 #b010)
  (define-stxparams #:errmsg "ERR" A B)
  (define a : Fixnum 0)
  (define b : Fixnum 0)
  (parameterize-syntax-ids
   (  [A a]
      [B b])
   (SET! A #b1111)
   (copy-bits #:from A #:to B /bit2 /bit1)
   (check-equal? B #b0110)
   (SET! A 0)
   (copy-bits #:from A #:to B /bit2 /bit1)
   (check-equal? B #b0000)
   (SET! A #b1111)
   (SET! B #b1000)
   (copy-bits #:from A #:to B /bit1)
   (check-equal? B #b1010)
   (SET! A 0)
   (copy-bits #:from A #:to B /bit1)
   (check-equal? B #b1000)
   ))
