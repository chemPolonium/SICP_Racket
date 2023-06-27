#lang racket

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value
           (logical-or (get-signal a1) (get-signal a2))])
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond [(and (= s1 1) (= s2 1)) 1]
        [(and (= s1 0) (= s2 0)) 0]
        [(and (= s1 1) (= s2 0)) 1]
        [(and (= s1 0) (= s2 1)) 1]
        [else (error "Invalid signal" s1 s2)]))

(define (get-signal wire) (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define after-delay 1)
(define or-gate-delay 1)