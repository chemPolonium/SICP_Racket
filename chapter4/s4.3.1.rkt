#lang sicp

(define (square x)
  (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
(define (smallest-divisor n)
  (define (next test-divisor)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 23)

(define (prime-sum-pair list1 list2)
  (let ([a (an-element-of list1)]
        [b (an-element-of list2)])
    (require (prime? (+ a b)))
    (list a b)))

(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(prime-sum-pair '(1 3 5 8) '(20 35 110))
