#lang racket

(require "common_lib.rkt")

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt0 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))
((deriv cube) 5)

;newton-transform makes a function from a function g,
;which transform x to zero-point of g a little
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
;fixed-point means there is no more improvement to make
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt1 x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt2 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt3 x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

(display "Exercise 1.40")
(define (cubic a b c)
  (lambda (x) (+ (* x (+ (* x (+ x a)) b)) c)))

(display "Exercise 1.41\n")
(define (double f)
  (lambda (x) (f (f x))))

;(double double) = double.double
;(double double.double) = double.double.double.double
;double.double.double.double(inc)
; =double.double.double(inc2)
; =double.double(inc4)
; =double.(inc8)
; =(inc16)
(((double (double double)) inc) 5)

(display "Exercise 1.42\n")
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(display "Exercise 1.43\n")
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

(display "Exercise 1.44\n")
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;damps required: (floor (log n 2))
(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (log n 2)))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))
(nth-root 2 2)
(nth-root 5 5)

(display "Exercise 1.46\n")
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 2)

(define (fixed-point0 f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) 0.001))
  ((iterative-improve good-enough? f) first-guess))