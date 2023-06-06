#lang sicp

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque node)
  (set-car! deque node))
(define (set-rear-ptr! deque node)
  (set-cdr! deque node))

; node : (cons item (cons prev-node next-node))

(define (prev-ptr node) (cadr node))
(define (next-ptr node) (cddr node))

; item is another node
(define (set-prev-ptr! node item)
  (set-car! (cdr node) item))
(define (set-next-ptr! node item)
  (set-cdr! (cdr node) item))

(define (make-deque) (cons nil nil))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque) (car (front-ptr deque)))

(define (rear-deque deque) (car (rear-ptr deque)))

(define (front-insert-deque! deque item)
  (let ([new-node (cons item (cons nil (front-ptr deque)))])
    (cond [(empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)]
          [else
           (set-prev-ptr! (front-ptr deque) new-node)
           (set-front-ptr! deque new-node)]))
  (print-deque deque))

(define (rear-insert-deque! deque item)
  (let ([new-node (cons item (cons (rear-ptr deque) nil))])
    (cond [(empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)]
          [else
           (set-next-ptr! (rear-ptr deque) new-node)
           (set-rear-ptr! deque new-node)]))
  (print-deque deque))

(define (front-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty queue")]
        [else
         (set-front-ptr! deque (next-ptr (front-ptr deque)))
         (if (null? (front-ptr deque))
             (set-rear-ptr! deque nil)
             (set-prev-ptr! (front-ptr deque) nil))
         (print-deque deque)]))

(define (rear-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty queue")]
        [else
         (set-rear-ptr! deque (prev-ptr (rear-ptr deque)))
         (if (null? (rear-ptr deque))
             (set-front-ptr! deque nil)
             (set-next-ptr! (rear-ptr deque) nil))
         (print-deque deque)]))

(define (print-deque deque)
  (cond [(empty-deque? deque)
         (display "empty deque")
         (newline)]
        [else
         (display (front-deque deque))
         (let iter ([node (next-ptr (front-ptr deque))])
           (cond [(null? node)
                  (newline)]
                 [else
                  (display " ")
                  (display (car node))
                  (iter (next-ptr node))]))]))

(define q2 (make-deque))
(front-insert-deque! q2 'a)
(front-insert-deque! q2 'b)
(rear-insert-deque! q2 'c)
(front-delete-deque! q2)
(rear-delete-deque! q2)
(rear-delete-deque! q2)