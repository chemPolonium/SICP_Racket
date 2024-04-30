#lang sicp

(define (require p) (if (not p) (amb)))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (all-pythagorean-triples low)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
