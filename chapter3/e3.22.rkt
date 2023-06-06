#lang sicp

(define (make-queue)
  (let ([front-ptr nil]
        [rear-ptr nil])
    (define (this)
      (cons front-ptr rear-ptr))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ([new-pair (cons item '())])
        (cond [(empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)]
              [else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)])
        (this)))
    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "DELETE! called with an empty queue")]
            [else
             (set-front-ptr! (cdr front-ptr))
             (this)]))
    (define (print-queue)
      (cond [(empty-queue?)
             (display "empty queue")
             (newline)]
            [else
             (display (car front-ptr))
             (let loop ([p (cdr front-ptr)])
               (if (pair? p)
                   (begin
                     (display " ")
                     (display (car p))
                     (loop (cdr p)))
                   (newline)))]))
    (define (dispatch m)
      (cond [(eq? m 'front-queue) front-queue]
            [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) delete-queue!]
            [(eq? m 'print-queue) print-queue]
            [else (error "not a valid operation")]))
    dispatch))

(define (front-queue queue)
  ((queue 'front-queue)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (print-queue queue)
  ((queue 'print-queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)