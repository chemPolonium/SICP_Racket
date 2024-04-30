#lang sicp

(define (require p) (if (not p) (amb)))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))
