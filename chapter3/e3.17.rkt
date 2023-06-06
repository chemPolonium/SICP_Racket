#lang sicp

; make a checker keeping a list of every checked element
(define (valid?-generator)
  ; l will not be released
  (let ([l (list)])
    (lambda (x)
      (if (memq x l)
          ; l contains the element means this element is not valid
          #f
          (begin
            ; add this element to l
            (set! l (cons x l))
            ; this element is valid
            #t)))))

(define valid? (valid?-generator))

(define (count-pairs x)
  (if (and (pair? x) (valid? x))
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)
      0))

(define x (cons 'a 'b))

(define return-3 (cons 'a (cons 'b (cons 'c nil))))
; return 3
(count-pairs return-3)

(define return-4 (cons x (cons x 'c)))
; return 3
(count-pairs return-4)

(define return-7 (cons (cons x x) (cons x x)))
; return 3
(count-pairs return-7)

(define x1 (cons 'a 'b))
(define x2 (cons 'a 'b))
(define x3 (cons 'a 'b))
(set-cdr! x1 x2)
(set-cdr! x2 x3)
(set-cdr! x3 x1)
(define return-never x1)
; return 3
(count-pairs return-never)