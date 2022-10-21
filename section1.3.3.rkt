#lang racket

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (average a b) (/ (+ a b) 2))

(define (nagative? x) (< x 0))
(define (positive? x) (> x 0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (nagative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sigh" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

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

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(display "Exercise 1.35\n")
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)

(display "Exercise 1.36\n")
(define (fixed-point-display f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display "guess:")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try (average guess next)))))
  (try first-guess))

(fixed-point-display (lambda (x) (/ (log 1000) (log x)))
                     4.55)

(display "Exercise 1.37\n")
(define (cont-frac-recurse n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (iter (+ 1 i))))))
  (iter 1))
(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) res)))))
  (iter k 0))
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           3)

(display "Exercise 1.38\n")
(define (e-euler k)
  (+ 2.0
     (cont-frac (lambda (i) 1)
                (lambda (i) (if (= 2 (remainder i 3))
                                (* 2 (/ (+ i 1) 3))
                                1))
                k)))
(e-euler 10)

(display "Exercise 1.39\n")
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))
(tan-cf 1.0 10)