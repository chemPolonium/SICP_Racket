#lang racket

(define one-through-four0
  (cons 1
        (cons 2
              (cons 3
                    (cons 4 null)))))
(define one-through-four (list 1 2 3 4))
one-through-four
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cadr one-through-four)
(cons 10 one-through-four)
(cons 5 one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length0 items)
  (if (null? items)
      0
      (+ 1 (length0 (cdr items)))))

(define odds (list 1 3 5 7))
(length0 odds)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a ) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(display "Exercise 2.17\n")
(define (last-pair l)
  (let ((rem (cdr l)))
    (if (null? rem)
        (car l)
        (last-pair rem))))

(last-pair (list 23 72 149 34))

(display "Exercise 2.18\n")
(define (reverse l)
  (define (iter r l)
    (if (null? l)
        r
        (iter (cons (car l) r) (cdr l))))
  (iter null l))

(reverse (list 1 4 9 16 25))

(display "Exercise 2.19\n")
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(cc 100 us-coins)
(define unsorted-us-coins (list 25 50 5 10 1))
(cc 100 unsorted-us-coins)

(display "Exercise 2.20\n")
(define (same-parity x . xs)
  (define same-parity-with-first?
    (if (odd? x)
        odd?
        even?))
  (define (select-xs xs)
    (cond ((null? xs) null)
          ((same-parity-with-first? (car xs))
           (cons (car xs)
                 (select-xs (cdr xs))))
          (else (select-xs (cdr xs)))))
  (cons x (select-xs xs)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(define (scale-list0 items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list0 (cdr items)
                         factor))))

(scale-list0 (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x)
  (* x x))

(map abs (list -10 2.5 -11.6 17))
(map square (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(display "Exercise 2.21\n")
(define (square-list0 items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list0 (cdr items)))))
(define (square-list items)
  (map square items))

(square-list0 (list 1 2 3 4))
(square-list (list 1 2 3 4))

(display "Exercise 2.22\n")
(display "Explained in comments\n")
; (cons (square (car things))
;       answer)
; makes '((square (car things)) answer)
; answer should be before the rest

; (cons answer (square (car things)))
; makes (((s1 s2) s3) s4)

(display "Exercise 2.23\n")
(define (for-each f items)
  (cond ((null? items) #t)
        (else (f (car items))
              (for-each f (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))