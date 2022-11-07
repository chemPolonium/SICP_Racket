#lang racket

(require racket/trace)

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
         (union-set-ul (cdr set1) set2)]
        [else
         (union-set-ul (cdr set1)
                       (cons (car set1) set2))]))

(displayln "Exercise 2.60")

(define (element-of-set-uldup? x set)
  (element-of-set-ul? x set))

(define (adjion-set-uldup x set)
  (cons x set))

(define (union-set-uldup set1 set2)
  (append set1 set2))

(define (intersection-set-uldup set1 set2)
  (intersection-set-ul set1 set2))

(displayln "Sets as ordered lists")

(define (element-of-set-ol? x set)
  (cond [(null? set) false]
        [(= x (car set)) true]
        [(< x (car set)) false]
        [else (element-of-set-ol? x (cdr set))]))

(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond [(= x1 x2)
               (cons x1 (intersection-set-ol (cdr set1)
                                             (cdr set2)))]
              [(< x1 x2)
               (intersection-set-ol (cdr set1) set2)]
              [(< x2 x1)
               (intersection-set-ol set1 (cdr set2))]))))

(displayln "Exercise 2.61")

(define (adjion-set-ol x set)
  (cond [(or (null? set)
             (< x (car set)))
         (cons x set)]
        [(= x (car set))
         set]
        [else (cons (car set)
                    (adjion-set-ol x (cdr set)))]))

(adjion-set-ol 5 '(3 4 6 7))

(displayln "Exercise 2.62")

(define (union-set-ol set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([x1 (car set1)]
                    [x2 (car set2)])
                (cond [(< x1 x2)
                       (cons x1
                             (union-set-ol (cdr set1) set2))]
                      [(> x1 x2)
                       (cons x2
                             (union-set-ol set1 (cdr set2)))]
                      [else
                       (cons x1
                             (union-set-ol (cdr set1) (cdr set2)))]))]))

(union-set-ol '(1 3 6) '(2 3 5))

(displayln "Sets as binary trees")
