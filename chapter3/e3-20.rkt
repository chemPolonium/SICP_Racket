#lang sicp

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond [(eq? m 'car) x]
          [(eq? m 'cdr) y]
          [(eq? m 'set-car!) set-x!]
          [(eq? m 'set-cdr!) set-y!]
          [else
           (error "Undefined operation: CONS" m)]))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value) z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value) z)

; creates a environment
(define x (cons 1 2))
; creates another environment with two x
(define z (cons x x))
; call set-car! in x
(set-car! (cdr z) 17)
(car x)
(car (car z))