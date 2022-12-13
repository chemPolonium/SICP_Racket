#lang racket

(displayln "Section 2.3.2 Example: Symbolic Differentiation")

(displayln "The differentiation program with abstract data")

(define (deriv-a exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum-a (deriv-a (addend exp) var)
                                (deriv-a (augend-a exp) var)))
        ((product? exp)
         (make-sum-a
          (make-product-a (multiplier exp)
                          (deriv-a (multiplicand-a exp) var))
          (make-product-a (deriv-a (multiplier exp) var)
                          (multiplicand-a exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(displayln "Representing algebraic expressions")

(define (variable? x)
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
; make-sum-old will not simplify the expression
(define (make-sum-a a1 a2)
  (list '+ a1 a2))
; make-product-old will not simplify the expression
(define (make-product-a m1 m2)
  (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s)
  (cadr s))
(define (augend-a s)
  (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p)
  (cadr p))
(define (multiplicand-a p)
  (caddr p))

(deriv-a '(+ x 3) 'x)
(deriv-a '(* x y) 'x)
(deriv-a '(* (* x y) (+ x 3)) 'x)
(deriv-a '(* y 3) 'x)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv-b exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv-b (addend exp) var)
                              (deriv-b (augend-a exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-b (multiplicand-a exp) var))
          (make-product (deriv-b (multiplier exp) var)
                        (multiplicand-a exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv-b '(+ x 3) 'x)
(deriv-b '(* x y) 'x)
(deriv-b '(* (* x y) (+ x 3)) 'x)
(deriv-b '(* y 3) 'x)

(displayln "Exercise 2.56")
(define (make-exponentiation a b)
  (cond ((=number? b 0) 1)
        ((=number? b 1) a)
        (else (list '** a b))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e)
  (cadr e))
(define (exponent e)
  (caddr e))

(define (deriv-c exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv-c (addend exp) var)
                              (deriv-c (augend-a exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-c (multiplicand-a exp) var))
          (make-product (deriv-c (multiplier exp) var)
                        (multiplicand-a exp))))
        ((exponentiation? exp)
         (let ([n (exponent exp)]
               [u (base exp)])
           (make-product (make-product n
                                       (make-exponentiation u (- n 1)))
                         (deriv-c u var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv-c '(** x 3) 'x)
(deriv-c '(* y (** x 3)) 'x)

(displayln "Exercise 2.57")
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (foldl make-sum 0 (cddr s))))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (foldl make-product 1 (cddr p))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ([n (exponent exp)]
               [u (base exp)])
           (make-product (make-product n
                                       (make-exponentiation u (- n 1)))
                         (deriv u var))))
        (else
         (error "unknown expression type: DERIV" exp))))
(deriv '(+ x x x x) 'x)
(deriv '(* x x x x) 'x)
(deriv '(* x y (+ x 3)) 'x)

(displayln "Exercise 2.58 a")
(define (infix->prefix exp)
  (cond [(number? exp) exp]
        [(variable? exp) exp]
        [else (list (cadr exp)
                    (infix->prefix (car exp))
                    (infix->prefix (caddr exp)))]))

(displayln "Exercise 2.58 b")
(define (add-brackets exp)
  (cond ((number? exp) exp)
        ((variable? exp) exp)
        ((= (length exp) 1) (car exp))
        ((eq? (cadr exp) '*)
         (add-brackets  (cons (list (add-brackets (car exp))
                                    '*
                                    (add-brackets (caddr exp)))
                              (cdddr exp))))
        ((eq? (cadr exp) '+)
         (list (add-brackets (car exp))
               '+
               (add-brackets (cddr exp))))
        (else
         (error "invalid function" exp))))

(define (prefix->infix exp)
  (cond [(number? exp) exp]
        [(variable? exp) exp]
        [else (list (prefix->infix (cadr exp))
                    (car exp)
                    (prefix->infix (caddr exp)))]))

(add-brackets '(2 * 4 + 1))
(add-brackets '(2 + 4 * 1))
(add-brackets '(2 + 2 + 2))
(add-brackets '(x + 3 * (x + y + 2)))

(infix->prefix (add-brackets '(x + 3 * (x + y + 2))))

(define (deriv-infix exp var)
  (deriv (infix->prefix exp) var))
(define (deriv-std exp var)
  (deriv-infix (add-brackets exp) var))
(define (deriv-std->std exp var)
  (prefix->infix (deriv-std exp var)))

(deriv-std '(x + 3 * (x + y + 2)) 'x)
(deriv-std '(x + 3 * x * y + y + 2) 'x)

(deriv-std->std '(x + 3 * x * y + y + 2) 'x)
(deriv-std->std '(x + 3 * x * y * x + y + 2) 'x)