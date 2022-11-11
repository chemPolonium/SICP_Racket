#lang racket

(displayln "Representing Huffman trees")

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(displayln "The decoding procedure")

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch
               (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "bad bit: CHOOSE-BRANCH" bit)]))

(displayln "Sets of weighted elements")

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set)))
         (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

(displayln "Exercise 2.67")

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(displayln "Exercise 2.68")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol c tree)
  (cond [(leaf? tree) null]
        [(member c (symbols (left-branch tree)))
         (cons 0 (encode-symbol c (left-branch tree)))]
        [else
         (cons 1 (encode-symbol c (right-branch tree)))]))

(define sample-message-to-encode '(A D A B B C A))

(encode sample-message-to-encode sample-tree)

(displayln "Exercise 2.69")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (let ([res (adjoin-set (make-code-tree (car leaf-set)
                                         (cadr leaf-set))
                         (cddr leaf-set))])
    (if (> (length leaf-set) 2)
        (successive-merge res)
        res)))

; this will be a little tricky
; this one cannot work on '((A 4) (B 3) (C 3) (D 2))
; it will leave A at top but wrong
; (define (successive-merge leaf-set)
;   (foldl make-code-tree (car leaf-set) (cdr leaf-set)))

; (define sample-pairs '((A 4) (B 2) (C 1) (D 1)))
(define sample-pairs '((A 4) (B 3) (C 3) (D 2)))

(generate-huffman-tree sample-pairs)

(displayln "Exercise 2.70")

(define rock-pairs
  '((NA 16) (YIP 9) (SHA 3) (GET 2) (A 2) (JOB 2) (WAH 1) (BOOM 1)))

(define sample-rock-lyric
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(define rock-tree (generate-huffman-tree rock-pairs))

(define sample-rock-lyric-encoded (encode sample-rock-lyric rock-tree))

(decode sample-rock-lyric-encoded rock-tree)

(displayln "Exercise 2.71")

; most frequent 1
; least frequent 5
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))

; most frequent 1
; least frequent 5
(generate-huffman-tree
 '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))

(displayln "Exercise 2.72")

; best O(1)
; worst O(n)