#lang racket

(require "common_lib.rkt")

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(display "Exercise 2.2\n")
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (make-point 5 6))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (mid-point segment)
  (let ((a (start-segment segment))
        (b (end-segment segment)))
    (make-point (average (x-point a)
                         (x-point b))
                (average (y-point a)
                         (y-point b)))))

(print-point (mid-point (make-segment (make-point 5.0 3.0)
                                      (make-point 2.0 8.0))))

(display "Exercise 2.3\n")
(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))
(define (bottom-left rect)
  (car rect))
(define (top-right rect)
  (cdr rect))
(define (bottom rect)
  (y-point (bottom-left rect)))
(define (top rect)
  (y-point (top-right rect)))
(define (left rect)
  (x-point (bottom-left rect)))
(define (right rect)
  (x-point (top-right rect)))
(define (bottom-right rect)
  (make-point (right rect)
              (bottom rect)))
(define (top-left rect)
  (make-point (left rect)
              (top rect)))

(define (width-rect rect)
  (- (right rect) (left rect)))

(define (height-rect rect)
  (- (top rect) (bottom rect)))

(define (area-rect rect)
  (* (width-rect rect)
     (height-rect rect)))

(define (perimeter-rect rect)
  (* 2 (+ (width-rect rect)
          (height-rect rect))))

(define r (make-rectangle (make-point 3 4)
                     (make-point 5 8)))

(area-rect r)
(perimeter-rect r)