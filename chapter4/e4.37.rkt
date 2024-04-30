#lang sicp

(define (require p) (if (not p) (amb)))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        ; k is directly determined
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))
