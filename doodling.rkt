#lang racket

(provide get-branch-dest)

(require racket/require
         (filtered-in
          (λ (name) ; Whenever I say `fx+` I actually mean `unsafe-fx+`
            (and (regexp-match #rx"^unsafe-fx" name)
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops))

(define max-fixnum ; : Fixnum
  (let loop ([doubler 256]); : Integer 256])
    (let ([temp (* 2 doubler)])
      (if (fixnum? temp)
          (loop temp)
          (- temp 1)))))

(define (~h val [width #f]); [val : Fixnum] [width : (U False Exact-Nonnegative-Integer) #f])
  (~a (string-upcase (format "~x" val))
      #:width width #:align 'right #:left-pad-string "0"))

;(~h max-fixnum)
;(~h (fx+ 1 max-fixnum))
;(~h (fx+ max-fixnum max-fixnum))

; Branch instructions
;   "contain a signed 8 bit relative offset (e.g. -128 to +127) which is added to program counter if the condition is true.
;   As the program counter itself is incremented during instruction execution by two
;   the effective address range for the target instruction must be with -126 to +129 bytes of the branch."
(define-syntax-rule (get-branch-dest .src .offset)
  (let* ([src .src]
         [offset .offset]
         [offset (if (fx= 0 (fxand #x80 offset))
                     offset
                     (fxior #xFF00 offset))])
    (fxand #xFFFF (fx+ src offset))))

(module+ test
  (require rackunit)

  ; Simple example with positive branch
  ; log item: C9F8  F0 07     BEQ $CA01                       A:80 X:00 Y:00 P:A5 SP:FB PPU:  6,174 CYC:740
  (check-equal? (get-branch-dest #xC9FA #x07)
                #xCA01)

  ; Negative branch (E0 should subtract)
  ; log item: C72A  D0 E0     BNE $C70C                       A:CB X:04 Y:4F P:6D SP:F1 PPU:129,303 CYC:14764
  ; C72C 1100 0111 0010 1100 (pc, adjusted+2)
  ;   E0           1110 0000 (offset)
  ; C80C 1100 1000 0000 1100 (sum)
  ; C70C 1100 0111 0000 1100 (desired, pc.hi|sum.lo)
  (check-equal? (get-branch-dest #xC72C #xE0)
                #xC70C)
  ; Hmm, it could also be calculated as
  #;(fx- pc (fx- #x100 offset))
  #;(fx- #xC72C (fx- #x100 #xE0))
  #;(fx- #xC72C #x20) ; => C70C

  ; Small positive branch that crosses page boundary
  ; log item: F2FC  F0 02     BEQ $F300                       A:52 X:02 Y:E9 P:67 SP:FB PPU:205, 19 CYC:23308
  ; F2FE 1111 0010 1111 1110 (pc, adjusted+2)
  ;   02           0000 0010 (offset)
  ; F300 1111 0011 0000 0000 (sum)
  ; F300 1111 0011 0000 0000 (desired)
  (check-equal? (get-branch-dest #xF2FE #x02)
                #xF300)

  ; Bug found during PPU development:
  (check-equal? (get-branch-dest #xF240 #xBB)
                #xF1FB)
  )
