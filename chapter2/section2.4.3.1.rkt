#lang racket

(displayln "Message passing")

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond [(eq? op 'real-part) x]
          [(eq? op 'imag-part) y]
          [(eq? op 'magnitude) (sqrt (+ (square x) (square y)))]
          [(eq? op 'angle) (atan y x)]
          [else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)]))
  dispatch)

(define (apply-generic op arg) (arg op))

(displayln "Exercise 2.75")

(define (make-from-mag-ang r a)
  (lambda (op)
    (cond [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [else (error "Unknown op: MAKE-FROM-MAG-ANG" op)])))

(displayln "Exercise 2.76")
; new types must often be added: message-passing-style
; do not need to writing install

; new operations must often be added: data-directed-style
; do not need to write new op for all datas