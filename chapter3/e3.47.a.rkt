#lang sicp

(define (make-list k v)
  (if (null? k)
      nil
      (cons v (make-list (dec k) v))))

(define (test-all mutexs)
  (cond [(null? mutexs) true]
        [((car mutexs) 'acquire)
         (test-all (cdr mutexs))]
        [else false]))

(define (make-semaphore n)
  (let ([mutexs (make-list n (make-mutex))])
    (define (the-semaphore m)
      (cond [(eq? m 'acqure)
             (if (test-all mutexs)
                 (1))]))
    the-semaphore))

(define (make-mutex)
  (let ([cell (list false)])
    (define (the-mutex m)
      (cond [(eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))]
            [(eq? m 'release) (clear! cell)]))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin
        (set-car! cell true)
        false)))
