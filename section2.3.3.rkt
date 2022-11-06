#lang racket

(displayln "Section 2.3.3 Example: Representing Sets")

(displayln "Sets as unordered lists")

(define (element-of-set-ul? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-ul? x (cdr set)))))

(define (adjion-set-ul x set)
  (if (element-of-set-ul? x set)
      set
      (cons x set)))

(define (intersection-set-ul set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set-ul? (car set1) set2)
         (cons (car set1) (intersection-set-ul (cdr set1) set2))]
        [else (intersection-set-ul (cdr set1) set2)]))

(displayln "Exercise 2.59")
(define (union-set-ul set1 set2)
  (cond [(null? set1) set2]
        [(element-of-set-ul? (car set1) set2)
         (union-set-ul (cdr set1)
                    set2)]
        [else
         (intersection-set-ul (cdr set1)
                           (cons (car set1)
                                 set2))]))

(displayln "Exercise 2.60")
(define (element-of-set-uldup? x set)
  (element-of-set-ul? x set))
(define (adjion-set-uldup x set)
  (cons x set))
(define (union-set-uldup set1 set2)
  (append set1 set2))
(define (intersection-set-uldup set1 set2)
  (intersection-set-ul set1 set2))