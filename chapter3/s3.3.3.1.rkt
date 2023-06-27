#lang sicp

(define (lookup key table)
  (let ([record (assoc key (cdr table))])
    (and record (cdr record))))

(define (assoc key records)
  (cond [(null? records) false]
        [(equal? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))

(define (insert! key value table)
  (let ([record (assoc key (cdr table))])
    (if record
        (set-cdr! record table)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup-2 key-1 key-2 table)
  (let ([subtable (assoc key-1 (cdr table))])
    (and subtable
         (let ([record (assoc key-2 (cdr subtable))])
           (and record (cdr record))))))

(define (insert-2! key-1 key-2 value table)
  (let ([subtable (assoc key-1 (cdr table))])
    (if subtable
        (let ([record (assoc key-2 (cdr subtable))])
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)