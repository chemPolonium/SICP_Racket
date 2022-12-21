#lang racket

(define (make-monitored f)
  (let ([t 0])
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          t
          (begin (set! t (add1 t))
                 (f x))))))

(define s (make-monitored add1))

(s 100)

(s 50)

(s 'how-many-calls?)