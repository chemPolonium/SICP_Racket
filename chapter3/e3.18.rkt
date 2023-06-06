#lang sicp

; the exercise only says taking successive cdrs
; but this can detect car and cdr loops
(define (contains-cycle? x)
  (define (cc-2 x l)
    (if (not (pair? x))
        #f
        (or (memq x l)
            (cc-2 (car x) (cons x l))
            (cc-2 (cdr x) (cons x l)))))
  (and (cc-2 x nil) #t))

(define x (cons 'a 'b))

(define return-3 (cons 'a (cons 'b (cons 'c nil))))

(define return-4 (cons x (cons x 'c)))

(define return-7 (cons (cons x x) (cons x x)))

(define x1 (cons 'a 'b))
(define x2 (cons 'a 'b))
(define x3 (cons 'a 'b))
(set-cdr! x1 x2)
(set-cdr! x2 x3)
(set-cdr! x3 x1)
(define return-never x1)

(contains-cycle? return-3)
(contains-cycle? return-4)
(contains-cycle? return-7)
(contains-cycle? return-never)