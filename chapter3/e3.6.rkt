#lang racket

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 50)

(define rand
  (let ([x random-init])
    (lambda (op)
      (cond [(eq? op 'generate)
             (set! x (rand-update x))
             x]
            [(eq? op 'reset)
             (lambda (v) (set! x v))]
            [else (error "not a valid operation")]))))

(rand 'generate)
(rand 'generate)

((rand 'reset) 10)
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 10)
(rand 'generate)
(rand 'generate)
(rand 'generate)
