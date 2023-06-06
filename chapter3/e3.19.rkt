#lang sicp

; only detects cdr
(define (contains-cycle? x)
  ; sometimes fast jumps too fast
  (define (safe-cdr x)
    (if (pair? x)
        (cdr x)
        nil))
  (define (cc-2 slow fast)
    (cond [(null? slow) #f] ; slow jump to nil
          [(null? fast) #f] ; fast jump to nil
          [(eq? slow fast) #t] ; fast meets slow
          ; slow jumps 1, fast jumps 2
          [else (cc-2 (safe-cdr slow) (safe-cdr (safe-cdr fast)))]))
  (cc-2 x (safe-cdr x)))

(define x (cons 'a 'b))

(define return-3 (cons 'a (cons 'b (cons 'c nil))))

(define return-4 (cons x (cons x 'c)))

(define return-7 (cons (cons x x) (cons x x)))

(define x1 (cons 'a 'b))
(define x2 (cons 'a 'b))
(define x3 (cons 'a 'b))
(set-cdr! x1 x2)
(set-cdr! x2 x3)
(set-cdr! x3 x1)
(define return-never x1)

(contains-cycle? return-3)
(contains-cycle? return-4)
(contains-cycle? return-7)
(contains-cycle? return-never)