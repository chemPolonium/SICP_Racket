#lang sicp

(define (make-table same-key?)
  (define (assoc key records)
    (cond [(null? records) false]
          [(same-key? key (caar records)) (car records)]
          [else (assoc key (cdr records))]))
  (let ([local-table (list '*table* 'base-val)])
    ; '(*table* base-val sub sub ...)
    ; subtable/record '(key val sub sub ...)
    ; base-val is to make sure base table has the same struct with subtable
    (define (lookup key-list)
      (let loop ([key-list key-list] [this-table local-table])
        (if (null? key-list)
            (cadr this-table)
            ; if this-table has no subtable, (cddr this-table) will be '()
            ; then subtable returned by assoc will be false
            (let ([subtable (assoc (car key-list) (cddr this-table))])
              (and subtable
                   (loop (cdr key-list) subtable))))))
    (define (insert! key-list value)
      (let loop ([key-list key-list] [this-table local-table])
        (if (null? key-list)
            (set-car! (cdr this-table) value)
            (let* ([this-key (car key-list)]
                   [subtable (assoc this-key (cddr this-table))])
              (if subtable
                  (loop (cdr key-list) subtable)
                  (begin
                    ; create a subtable with this-key
                    (set-cdr! (cdr this-table)
                              (cons (list this-key false)
                                    (cddr this-table)))
                    (loop (cdr key-list) (caddr this-table)))))))
      'ok)
    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [else (error "Unknown operation: TABLE" m)]))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(a) 3)
(put '(a b) 4)
(put '(a c) 5)
(put '(a b d) 6)
(get '(a))
(get '(a b))
(get '(a c))
(get '(a b d))
(get '(a d))