#lang racket

(require "common_lib.rkt")
(require math/number-theory)

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
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x)
                   this-coeff))
              0
              coefficient-sequence))

(let ((x 2))
  (+ 1
     (* 3 x)
     (* 5 (expt x 3))
     (expt x 5)))
(horner-eval 2 (list 1 3 0 5 0 1))

(displayln "Exercise 2.35")
(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (x)
          (cond ((null? x) 0)
                ((list? x) (count-leaves x))
                (else 1)))
        t)))

(count-leaves '(1 2 () () (3 ((2 3) 1) ())))

(displayln "Exercise 2.36")
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(displayln "Exercise 2.37")
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mi)
         (dot-product mi v))
       m))

(define (transpose m)
  (accumulate-n cons null m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi) matrix-*-vector cols mi) m)))

(let ((v '(1 2 3 4))
      (w '(5 6 7 8))
      (m '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
      (n '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  (displayln (dot-product v w))
  (displayln (matrix-*-vector m v))
  (displayln (transpose m))
  (displayln (matrix-*-matrix m n)))

(displayln "Exercise 2.38")
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list null (list 1 2 3))
(fold-left list null (list 1 2 3))

(displayln "(op (op a b) c) = (op a (op b c))")
(displayln "example: a + (b + c) = (a + b) + c")
(fold-right + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))

(displayln "Exercise 2.39")
(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (cond ((null? y) x)
                                  ((not (pair? y)) (list y x))
                                  (else (append y (list x)))))
              null
              sequence))
(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x))
             null
             sequence))
(reverse-fold-right '(1 2 3 4 5))
(reverse-fold-left '(1 2 3 4 5))

(displayln "Nested Mappings")

; (accumulate
;  append null (map (lambda (i)
;                     (map (lambda (j) (list i j))
;                          (enumerate-interval 1 (-i 1))))
;                   (enumerate-interval 1 6)))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs0 n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(prime-sum-pairs0 6)

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(displayln "Exercise 2.40")
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs 6)

(displayln "Exercise 2.41")
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (kj) (list (cadr kj) (car kj) i))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (ordered-triples-sum-s n s)
  (filter (lambda (x) (= (apply + x) s))
          (unique-triples n)))

(ordered-triples-sum-s 6 8)

(displayln "Exercise 2.42")
; (cons queen-k queen-o)
; insert new kth queen at begin of the list is a better way to make lists
; queen-k will be (car positions) and queen-o will be (cdr positions)
; the horz distance between queen-k and queen-o will be just a inc range
; and one more advantange: the answer will be symmertic, so there is no
;   need for reverse
(define (queens board-size)
  (define empty-board null)
  (define (safe? k positions)
    (let ((queen-k (car positions))
          (queen-o (cdr positions)))
      (let ((horz-occupied queen-o)
            ; there is no need to consider out of the board
            ; because out-of-board diag-occupied will just
            ; not conflict with the queen-k
            ; (we won't place queen-k out of the board)
            (diag-occupied (map + queen-o (range 1 k)))
            (adiag-occupied (map - queen-o (range 1 k))))
        (nor (member queen-k horz-occupied)
             (member queen-k diag-occupied)
             (member queen-k adiag-occupied)))))
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)

(displayln "Exercise 2.43")
(displayln "Explained in comments")
; this interchange deprives the ability of cutting the
; (queen-cols (- k 1)) off at queen-k. It will produce all the arranges
; and then judge them (in one arrange: one by one k in procedure)
; one by one.