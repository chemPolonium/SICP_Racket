#lang sicp

(define (peter balance)
  (+ balance 10))

(define (paul balance)
  (- balance 20))

(define (mary balance)
  (/ balance 2))

(define (combine . procs)
  (define (loop procs arg)
    (if (null? procs)
        arg
        (loop (cdr procs) ((car procs) arg))))
  (lambda (arg)
    (loop (reverse procs) arg)))

(define balance 100)

((combine peter paul mary) balance)
((combine peter mary paul) balance)
((combine paul peter mary) balance)
((combine paul mary peter) balance)
((combine mary peter paul) balance)
((combine mary paul peter) balance)
