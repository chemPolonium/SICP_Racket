#lang scheme

(define size 2)

(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

(define (square x) (* x x))
(square 21)

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs1 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs2 x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y) (or (> x y) (= x y)))
(define (>=1 x y) (not (< x y)))

(define (sum-of-square-of-two-largest x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= x y) (>= z y)) (sum-of-squares x z))
        ((and (>= y x) (>= z x)) (sum-of-squares y z))))
(sum-of-square-of-two-largest 3 2 4)

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (new-guess guess)
    (/ x guess))
  (define (improve guess)
    (average guess
             (new-guess guess)))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess))
       (* guess .0001)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(sqrt 2)

(define (cbrt x)
  (define (sqrt-iter guess x)
    (define (new-guess guess x)
      (/ (+ (/ x (square guess))
            (* 2 guess))
         3))
    (define (improve guess x)
      (average guess
               (new-guess guess x)))
    (define (good-enough? guess x)
      (< (abs (- (improve guess x) guess))
         (* guess .0001)))
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
(cbrt 8)
