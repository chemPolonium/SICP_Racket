#lang racket

(define f (let ([x 1])
            (lambda (y)
              (begin (set! x (* x y))
                     x))))

(+ (f 0) (f 1))