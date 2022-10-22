#lang racket

(define (make-interval a b)
  (cons a b))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(display "Exercise 2.7\n")
(display "defined at line 3\n")

(display "Exercise 2.8\n")
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(display "Exercise 2.9\n")
(display "in comment\n")
;(lower-bound (+ a b c))
; =(+ (lower-bound a)
;     (lower-bound b)
;     (lower-bound c))
(mul-interval (make-interval 1 2)
              (make-interval 3 4))
(div-interval (make-interval 1 2)
              (make-interval 3 4))

(display "Exercise 2.10\n")
(define (div-interval-checkzero x y)
  (if (and (>= (upper-bound y) 0)
           (<= (lower-bound y) 0))
      (error "divide by zero\n")
      (div-interval x y)))
;will cause error:
;(div-interval-checkzero (make-interval 1 2) (make-interval -1 1))

(display "Exercise 2.11\n")
