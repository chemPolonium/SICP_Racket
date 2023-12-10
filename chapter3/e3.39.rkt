#lang sicp

(define (p1 x)
  (* x x))

(define (p2 x)
  (+ x 1))

(define x 10)

; p1 -> p2
(p2 (p1 x))

; p2 -> p1
(p1 (p2 x))

; p1 -> p2 -> p1set
(p1 x)
