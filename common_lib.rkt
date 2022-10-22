#lang racket

(define (inc x)
  (+ x 1))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(provide inc average square fixed-point)