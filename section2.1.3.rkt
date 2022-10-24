#lang racket

(define (cons0 x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m)))))
(define (car0 z) (z 0))
(define (cdr0 z) (z 1))

(displayln "Exercise 2.4")
(define (cons1 x y)
  (lambda (m) (m x y)))
(define (car1 z)
  (z (lambda (p q) p)))
(define (cdr1 z)
  (z (lambda (p q) q)))

(car1 (cons1 3 5))
(cdr1 (cons1 3 5))

(displayln "Exercise 2.5")
(define (divisible-log p n)
  (if (not (= (remainder n p) 0))
      0
      (+ 1 (divisible-log p (/ n p)))))

(define (cons2 x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car2 z)
  (divisible-log 2 z))
(define (cdr2 z)
  (divisible-log 3 z))

(car2 (cons2 3 5))
(cdr2 (cons2 3 5))

(displayln "Exercise 2.6")
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))