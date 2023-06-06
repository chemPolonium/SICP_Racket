#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x (cons 'a 'b))

(define return-3 (cons 'a (cons 'b (cons 'c nil))))
(count-pairs return-3)

(define return-4 (cons x (cons x 'c)))
(count-pairs return-4)

(define return-7 (cons (cons x x) (cons x x)))
(count-pairs return-7)

(define x1 (cons 'a 'b))
(define x2 (cons 'a 'b))
(define x3 (cons 'a 'b))
(set-cdr! x1 x2)
(set-cdr! x2 x3)
(set-cdr! x3 x1)
(define return-never x1)
; this will never return
; (count-pairs return-never)