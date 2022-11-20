#lang racket

(define *complex-number-table* (make-hash))

(define (put-complex key1 key2 value)
  (hash-set! *complex-number-table* (list key1 key2) value))

(define (get-complex key1 key2)
  (hash-ref *complex-number-table* (list key1 key2) #f))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put-complex 'real-part '(rectangular) real-part)
  (put-complex 'imag-part '(rectangular) imag-part)
  (put-complex 'magnitude '(rectangular) magnitude)
  (put-complex 'angle '(rectangular) angle)
  (put-complex 'make-from-real-imag 'rectangular
               (lambda (x y) (tag (make-from-real-imag x y))))
  (put-complex 'make-from-mag-ang 'rectangular
               (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put-complex 'real-part '(polar) real-part)
  (put-complex 'imag-part '(polar) imag-part)
  (put-complex 'magnitude '(polar) magnitude)
  (put-complex 'angle '(polar) angle)
  (put-complex 'make-from-real-imag 'polar
               (lambda (x y) (tag (make-from-real-imag x y))))
  (put-complex 'make-from-mag-ang 'polar
               (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic-complex op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get-complex op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No methos for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic-complex 'real-part z))

(define (imag-part z) (apply-generic-complex 'imag-part z))

(define (magnitude z) (apply-generic-complex 'magnitude z))

(define (angle z) (apply-generic-complex 'angle z))

(define (make-from-real-imag x y)
  ((get-complex 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get-complex 'make-from-mag-ang 'polar) r a))

(install-rectangular-package)
(install-polar-package)

(displayln "Exercise 2.73")
; answer for a:
; a number or a variable does not have a "operator",
; so "operator" will not work

(define *deriv-table* (make-hash))

(define (put-deriv func type value)
  (hash-set! *deriv-table* (list func type) value))

(define (get-deriv func type)
  (hash-ref *deriv-table* (list func type) #f))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else ((get-deriv 'deriv (operator exp))
               (operands exp) var)]))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation a b)
  (cond ((=number? b 0) 1)
        ((=number? b 1) a)
        (else (list '** a b))))

; answer for b
(define (install-sum-package)
  ; ; here we used "deriv" from outside,
  ; ; so we have to define "deriv-sum",
  ; ; not "deriv"
  (define (addend s)
    (car s))
  (define (augend s)
    (if (null? (cddr s))
        (cadr s)
        (foldl make-sum 0 (cdr s))))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put-deriv 'deriv '+ deriv-sum)
  'done-install-sum)

(define (install-product-package)
  (define (multiplier p)
    (car p))
  (define (multiplicand p)
    (if (null? (cddr p))
        (cadr p)
        (foldl make-product 1 (cdr p))))
  (define (deriv-product exp var)
    (make-sum (make-product
               (multiplier exp)
               (deriv (multiplicand exp) var))
              (make-product
               (deriv (multiplier exp) var)
               (multiplicand exp))))
  (put-deriv 'deriv '* deriv-product)
  'done-install-product)

; answer for c
(define (install-exponentiation-package)
  (define (base e)
    (car e))
  (define (exponent e)
    (cadr e))
  (define (deriv-exponent exp var)
    (let ([n (exponent exp)]
          [u (base exp)])
      (make-product (make-product n
                                  (make-exponentiation u (sub1 n)))
                    (deriv u var))))
  (put-deriv 'deriv '** deriv-exponent)
  'done-install-exponent)

(install-sum-package)
(install-product-package)
(install-exponentiation-package)

; ; answer for d
; (define (get-deriv type func)
;   (hash-ref *deriv-table* (list func type) #f))
