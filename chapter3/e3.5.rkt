#lang racket

(define (monte-carlo trials experiment)
  (let iter ([trials-remaining trials]
             [trials-passed 0])
    (cond [(zero? trials-remaining)
           (/ trials-passed trials)]
          [(experiment)
           (iter (sub1 trials-remaining)
                 (add1 trials-passed))]
          [else
           (iter (sub1 trials-remaining)
                 trials-passed)])))

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (* range (random)))))

;;; upper bound x1 y1
;;; lower bound x2 y2
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (predicate-test)
    (predicate (random-in-range x2 x1)
               (random-in-range y2 y1)))
  (* (monte-carlo trials predicate-test)
     (- x1 x2)
     (- y1 y2)))

(define (square x)
  (* x x))

;;; calculate pi
(let ([circle-area
       (estimate-integral (lambda (x y)
                            (<= (+ (square (- x 5))
                                   (square (- y 7)))
                                (square 3)))
                          8 2 11 4 100000)])
  (exact->inexact (/ circle-area 9)))