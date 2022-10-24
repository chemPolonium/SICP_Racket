#lang racket

(require "common_lib.rkt")

(define (square x) (* x x))
(define (sum-odd-squares0 tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares0 (car tree))
                 (sum-odd-squares0 (cdr tree))))))

(define (even-fibs0 n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons null (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree)))))

(define (even-fibs n)
  (accumulate
   cons
   null
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   null
   (map square (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; (define (salary-of-highest-paid-programmer records)
;   (accumulate max 0 (map salary (filter programmer? records))))

(displayln "Exercise 2.33")
(define (map0 p sequence)
  (accumulate
   (lambda (x y)
     (cons (p x) y))
   null
   sequence))

(map0 square '(1 2 3 4 5))

(define (append0 seq1 seq2)
  (accumulate
   cons
   seq2
   seq1))

(append0 '(1 2 3) '(4 5 6))

(define (length0 sequence)
  (accumulate
   (lambda (x y) (+ y 1))
   0
   sequence))

(length0 `(1 2 3 4 5))

(displayln "Exercise 2.34")
