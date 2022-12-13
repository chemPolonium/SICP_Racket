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

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-bt? x set)
  (cond [(null? set) false]
        [(= x (entry set)) true]
        [(< x (entry set))
         (element-of-set-bt? x (left-branch set))]
        [(> x (entry set))
         (element-of-set-bt? x (right-branch set))]))

(define (adjoin-set-bt x set)
  (cond [(null? set) (make-tree x empty empty)]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-bt x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-bt x (right-branch set)))]))

(displayln "Exercise 2.63")

; the "append" in tree->list-1 is O(n), but "cons" in tree->list-1 is O(1)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define test-tree-1
  '(7 (3 (1 () ())
         (5 () ()))
      (9 ()
         (11 () ()))))
(tree->list-1 test-tree-1)
(tree->list-2 test-tree-1)

(define test-tree-2
  '(3 (1 () ())
      (7 (5 () ())
         (9 ()
            (11 () ())))))
(tree->list-1 test-tree-2)
(tree->list-2 test-tree-2)

(define test-tree-3
  '(5 (3 (1 () ())
         ())
      (9 (7 () ())
         (11 () ()))))
(tree->list-1 test-tree-3)
(tree->list-2 test-tree-3)

(displayln "Exercise 2.64")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  ; this is a O(n) procedure
  ; each operationg is O(1)
  ; each element is accessed once
  ; so it's O(n)
  (if (= n 0)
      ; elts is '(), so non-left-elts will be '()
      ; first '() is the left-tree
      (cons '() elts)
      ; consuming the left half elements
      (let* ([left-size (quotient (sub1 n) 2)]
             [left-result (partial-tree elts left-size)])
        ; parse the left result to left-tree and non-left-elts
        ; get the right-size by sub the current tree total size and the left-tree size
        (let ([left-tree (car left-result)]
              [non-left-elts (cdr left-result)]
              [right-size (- n 1 left-size)])
          ; the first non-left-elts will the the entry
          ; then the right-result can be calculated
          ; right-result is calculated using the right elements
          (let ([this-entry (car non-left-elts)]
                [right-result (partial-tree
                               (cdr non-left-elts)
                               right-size)])
            ; parse the right-result
            (let ([right-tree (car right-result)]
                  [remaining-elts (cdr right-result)])
              ; return (left-tree remaining-elts)
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts)))))))

(list->tree '(1 2 3 4 5 6 7))
;      4
;    /    \
;   2      6
;  / \    / \
; 1   3  5   7

(displayln "Exercise 2.65")

(define (union-set-bt set1 set2)
  (list->tree (union-set-ol (tree->list-2 set1)
                            (tree->list-2 set2))))

(define test-tree-4 (list->tree '(1 2 3 4 8 9)))
(define test-tree-5 (list->tree '(3 4 5 6 7 10)))

(union-set-bt test-tree-4 test-tree-5)
(tree->list-2 (union-set-bt test-tree-4 test-tree-5))

(displayln "Sets and information retrieval")

(define (make-record key value)
  (cons key value))

(define (key record)
  ((car record)))

(define (lookup-ul given-key set-of-records)
  (cond [(null? set-of-records) false]
        [(equal? given-key (key (car set-of-records)))
         (car set-of-records)]
        [else (lookup-ul given-key (cdr set-of-records))]))

(define (lookup-bd given-key set-of-records)
  (cond [(null? set-of-records) false]
        [(= given-key (key (entry set-of-records)))
         (entry set-of-records)]
        [(< given-key (key (entry set-of-records)))
         (lookup-bd given-key (left-branch set-of-records))]
        [(> given-key (key (entry set-of-records)))
         (lookup-bd given-key (right-branch set-of-records))]))