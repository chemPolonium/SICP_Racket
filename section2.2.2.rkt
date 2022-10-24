#lang racket

(define x (cons (list 1 2) (list 3 4)))
(length x)

(length (list x x))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)
(count-leaves (list x x))

(displayln "Exercise 2.24")
(list 1 (list 2 (list 3 4)))
;  (1 (2 (3 4)))
;  /           \
; 1         (2 (3 4))
;           /       \
;          2       (3 4)
;                  /   \
;                 3     4

(displayln "Exercise 2.25")
(cons 1 (cons 3 (cons (cons 5 (cons 7 null)) (cons 9 null))))
(let ((l '(1 3 (5 7) 9)))
  (car (cdr (car (cdr (cdr l))))))
(let ((l '((7))))
  (car (car l)))
(let ((l '(1 (2 (3 (4 (5 (6 7))))))))
  (cadr (cadr (cadr (cadr (cadr (cadr l)))))))

(displayln "Exercise 2.26")
(let ((x (list 1 2 3))
      (y (list 4 5 6)))
  (displayln (append x y))
  (displayln (cons x y))
  (displayln (list x y)))

(displayln "Exercise 2.27")
(define (deep-reverse x)
  (if (pair? x)
      (reverse (map deep-reverse x))
      x))
(let ((x (list (list 1 2) (list 3 4))))
  (displayln (reverse x))
  (displayln (deep-reverse x)))

(displayln "Exercise 2.28")
(define (fringe0 x)
  (cond ((null? x) null)
        ((pair? x) (append (fringe0 (car x)) (fringe0 (cdr x))))
        (else (list x))))
(let ((x (list (list 1 2) (list 3 4))))
  (displayln (fringe0 x))
  (displayln (fringe0 (list x x))))

(define (fringe items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? items) (iter (car items)
                               (iter (cdr items) result)))
          (else (cons items result))))
  (iter items null))

(let ((x (list (list 1 2) (list 3 4)))
      (f fringe))
  (displayln (f x))
  (displayln (f (list x x))))

(displayln "Exercise 2.29")
(define (make-mobile left right)
  (list left right))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (make-branch len structure)
  (list len structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile) (+ (total-weight (branch-structure (left-branch mobile)))
                           (total-weight (branch-structure (right-branch mobile)))))
        (else mobile)))

(let ((a (make-mobile (make-branch 2 3) (make-branch 2 3))))
  (total-weight a))

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (pair? mobile)
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))
      #t))

(let ((a (make-mobile (make-branch 10 (make-mobile (make-branch 2 3)
                                                   (make-branch 2 3)))
                      (make-branch 12 5))))
  (balanced? a))

(define (make-mobile-cons left right) (cons left right))
(define (make-branch-cons len structure) (cons len structure))
(define (left-branch-cons mobile) (car mobile))
(define (right-branch-cons mobile) (cdr mobile))
(define (branch-length-cons branch) (car branch))
(define (branch-structure-cons branch) (cdr branch))

(define (scale-tree0 tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree0 (car tree) factor)
                    (scale-tree0 (cdr tree) factor)))))
(scale-tree0 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree0 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(displayln "Exercise 2.30")
(define (square x) (* x x))
(define (square-tree0 tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree0 (car tree))
                    (square-tree0 (cdr tree))))))

(square-tree0
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree1 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree1 sub-tree)
             (square sub-tree)))
       tree))

(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(displayln "Exercise 2.31")
(define (tree-map f tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

(define (square-tree tree) (tree-map square tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(displayln "Exercise 2.32")
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(subsets '(1 2 3))