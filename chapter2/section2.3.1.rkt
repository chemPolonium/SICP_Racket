#lang racket

(displayln "Section 2.3: Symbolic Data")
(displayln "Section 2.3.1 Quotation")

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))
(cdr '(a b c))

(define (memq/user item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq/user item (cdr x)))))
; this will be false
(memq/user 'apple '(pear banana prune))
; this will be '(apple pear)
(memq/user 'apple '(x (apple sauce) y apple pear))

(displayln "Exercise 2.53")
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq/user 'red '((red shoes) (blue socks)))
(memq/user 'red '(red shoes blue socks))

(displayln "Exercise 2.54")
(define (equal?/user a b)
  (cond ((and (null? a) (null? b)) true)
        ((eq? (car a) (car b))
         (equal?/user (cdr a) (cdr b)))
        (else false)))
(equal?/user '(this is a list) '(this is a list))
(equal?/user '(this is a list) '(this (is a) list))

(displayln "Exercise 2.55")
(car ''aaaa)
(cadr ''aaaa)
(car '''aaaa)
(cadr '''aaaa)
(cdadr '''aaaa)
