#lang racket

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

;(define (linear-combination a b x y)
;  (add (mul a x) (mul b y)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer x) (denom x))))

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
(car (cdr z))

(define (make-rat0 n d) (cons n d))

(define (make-rat1 n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat2 n d)
  (let ((g (gcd n d))
        (absn (abs n))
        (absd (abs d)))
    (if (equal? (positive? n) (positive? d))
        (cons (/ absn g) (/ absd g))
        (cons (- (/ absn g)) (/ absd g)))))

(define (make-rat n d)
  (let ((g ((if (positive? d) + -) (gcd n d))))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
;1/2
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
;5/6
(print-rat (mul-rat one-half one-third))
;1/6
(print-rat (add-rat one-third one-third))
;6/9
(newline)

(display "Exercise 2.1\n")
(display "defined in line 51\n")
(print-rat (sub-rat one-third one-half))
(print-rat (make-rat 1 (- 2)))