#lang racket

(require racket/trace)

(define *table* (make-hash))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [error "Bad tagged datum: CONTENTS" datum]))

(define (put func type value)
  (hash-set! *table* (list func type) value))

(define (get func type)
  (hash-ref *table* (list func type)))

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

; from exercise 2.81a
(define (exp x y) (apply-generic 'exp x y))

; from Exercise 2.88
(define (neg x) (apply-generic 'neg x))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  ;; from exercise 2.79
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ;; from exercise 2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? x)))
  ;; from exercise 2.81a
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  ;; from Exercise 2.88
  (put 'neg '(scheme-number)
       (lambda (x) (- x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (and (negative? n) (negative? d))
        (make-rat (- n) (- d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; from Exercise 2.88
  (define (neg-rat x)
    (make-rat (- (numer x))
              (denom x)))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;; from exercise 2.79
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  ;; from exercise 2.80
  (define (=zero?-rat x)
    (zero? (numer x)))
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  ;; for exercise 2.83
  (put 'numer '(rational)
       (lambda (x) (numer x)))
  (put 'denom '(rational)
       (lambda (x) (denom x)))
  ;; from Exercise 2.88
  (put 'neg '(rational)
       (lambda (x) (neg-rat x)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define (square x) (* x x))

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
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
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
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; from Exercise 2.88
  (define (neg-complex z)
    (make-complex-from-real-imag (- (real-part z))
                                 (- (imag-part z))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; from exercise 2.79
  (define (equ?-complex z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex z1 z2)))
  ;; from exercise 2.80
  (define (=zero?-complex z)
    (and (zero? (real-part z))
         (zero? (imag-part z))))
  (put '=zero? '(complex)
       (lambda (z) (=zero?-complex z)))

  ;; from Exercise 2.88
  (put 'neg '(complex)
       (lambda (z) (neg-complex z)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

; Section 2.5.2 begins

(define *coercion-table* (make-hash))

(define (put-coercion from to value)
  (hash-set! *coercion-table* (list from to) value))

(define (get-coercion from to)
  (hash-ref *coercion-table* (list from to)))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

; from exercise 2.82
(define (apply-generic op . args)
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (let* ([new-args (map
                          (lambda (proc type) (proc type))
                          (findf
                           (lambda (l) (andmap identity l))
                           (map
                            (lambda (high-type)
                              (map
                               (lambda (low-type)
                                 (if (= high-type low-type)
                                     identity
                                     (get-coercion low-type high-type)))
                               (type-tags)))
                            (type-tags)))
                          args)]
               [new-type-tags (map type-tag new-args)]
               [new-proc (get op new-type-tags)])
          (if new-proc
              (apply new-proc (map contents new-args))
              (error "No method for these types"
                     (list op new-type-tags)))))))

(define (=zero? x) (apply-generic '=zero? x))

(displayln "Section 2.5.3 Example: Symbolic Algebra")

(define (install-term-package)
  ;; this is the term package for term-list

  ;; a term is '(list order coeff)
  (define (make-term order coeff)
    (cons order coeff))
  (define (order term)
    (car term))
  (define (coeff term)
    (cdr term))

  ;; external port
  (define (tag x) (attach-tag 'term x))
  (put 'make-term 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  (put 'order '(term)
       (lambda (term) (order term)))
  (put 'coeff '(term)
       (lambda (term) (coeff term)))
  'done)

(install-term-package)

(define (make-term order coeff)
  ((get 'make-term 'term) order coeff))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))

(define (install-dense-package)
  ;; this is the dense TERM-LIST package

  ;; basic term-list structure
  (define (make-dense term-list)
    term-list)
  (define (empty-termlist? l)
    (null? l))

  ;; extract the first term and rest terms
  (define (first-term-order term-list)
    (sub1 (length term-list)))
  (define (first-term-coeff term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))

  ;; adjoin a external term to the term-list
  (define (adjoin-term term term-list)
    (cond [(=zero? (coeff term)) term-list]
          [(= (order term) (length term-list)) (cons (coeff term) term-list)]
          [else (adjoin-term term (cons 0 term-list))]))

  ;; negative procedure
  (define (neg-dense term-list)
    (map neg term-list))

  ;; =zero? procedure
  (define (=zero?-dense term-list)
    (andmap =zero? term-list))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'dense x))
  (put 'make-from-dense 'dense
       (lambda (term-list) (tag (make-dense term-list))))
  (put 'empty-termlist? '(dense)
       (lambda (term-list) (empty-termlist? term-list)))
  (put 'first-term '(dense)
       (lambda (term-list) (make-term (first-term-order term-list)
                                      (first-term-coeff term-list))))
  (put 'rest-terms '(dense)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'adjoiner '(dense)
       (lambda (term-list)
         (lambda (term)
           (tag (adjoin-term term term-list)))))
  (put 'neg '(dense)
       (lambda (term-list) (tag (neg-dense term-list))))
  (put '=zero? '(dense)
       (lambda (term-list) (=zero?-dense term-list)))
  'done)

(define (install-sparse-package)
  ;; this is the sparse TERM-LIST package

  ;; basic term-list structure
  ;; (make-sparse '((3 2) (1 4)))
  (define (make-sparse term-list)
    term-list)
  (define (empty-termlist? l)
    (null? l))

  ;; internal term
  (define (make-internal-term order coeff)
    (list order coeff))
  (define (order-internal internal-term)
    (first internal-term))
  (define (coeff-internal internal-term)
    (second internal-term))

  ;; extract the first term and rest terms
  (define (internal-first-term term-list)
    (first term-list))
  (define (first-term-order term-list)
    (order-internal (internal-first-term term-list)))
  (define (first-term-coeff term-list)
    (coeff-internal (internal-first-term term-list)))
  (define (rest-terms term-list)
    (cdr term-list))

  ;; adjoin a term to the term-list
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons (make-internal-term (order term) (coeff term))
              term-list)))

  ;; negative procedure
  (define (neg-sparse term-list)
    (map (lambda (internal-term)
           (make-internal-term (order-internal internal-term)
                               (neg (coeff-internal internal-term))))
         term-list))

  ;; zero? procedure
  (define (=zero?-sparse term-list)
    (andmap (lambda (internal-term)
              (=zero? (coeff-internal internal-term)))
            term-list))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'sparse x))
  (put 'make-from-sparse 'sparse
       (lambda (term-list) (tag (make-sparse term-list))))
  (put 'empty-termlist? '(sparse)
       (lambda (term-list) (empty-termlist? term-list)))
  (put 'first-term '(sparse)
       (lambda (term-list) (make-term (first-term-order term-list)
                                      (first-term-coeff term-list))))
  (put 'rest-terms '(sparse)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'adjoiner '(sparse)
       (lambda (term-list)
         (lambda (term)
           (tag (adjoin-term term term-list)))))
  (put 'neg '(sparse)
       (lambda (term-list) (tag (neg-sparse term-list))))
  (put '=zero? '(sparse)
       (lambda (term-list) (=zero?-sparse term-list)))
  'done)

(define (install-polynomial-package)
  ;; the polynomial package

  ;; install the dense package
  (install-dense-package)

  ;; internal procedures to make dense term-list
  (define (make-dense-term-list original-term-list)
    ((get 'make-from-dense 'dense) original-term-list))
  (define (the-empty-dense-termlist) (make-dense-term-list '()))

  ;; install the sparse package
  (install-sparse-package)

  ;; internal procedures to make sparse term-list
  (define (make-sparse-term-list original-term-list)
    ((get 'make-from-sparse 'sparse) original-term-list))
  (define (the-empty-sparse-termlist) (make-sparse-term-list '()))

  ;; internal procedure to make a poly
  (define (make-poly variable term-list)
    (cons variable term-list))

  ;; procedures to get the variable and term-list
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; exposed procedure to make poly
  (define (make-poly-from-dense variable original-term-list)
    (make-poly variable (make-dense-term-list original-term-list)))
  (define (make-poly-from-sparse variable original-term-list)
    (make-poly variable (make-sparse-term-list original-term-list)))

  ;; internal procedure to get first term and rest terms from dense and sparse packages
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  (define (rest-terms term-list)
    (apply-generic 'rest-terms term-list))
  (define (empty-termlist? term-list)
    (apply-generic 'empty-termlist? term-list))

  ;; adjoin-term procedure is provided by dense and sparse package
  (define (adjoin-term term term-list)
    ((apply-generic 'adjoiner term-list) term))

  ;; internal peocedures
  (define (variable? x)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  ;; procedures used by add-poly
  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else
           (let ([t1 (first-term L1)]
                 [t2 (first-term L2)])
             (cond [(> (order t1) (order t2))
                    ;; t1 is the highest term in all
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2))]
                   [(< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2)))]
                   [else
                    (adjoin-term
                     ;; both t1 and t2 are highest terms
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))]))]))

  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  (define (neg-poly p)
    (make-poly (variable p)
               (neg (term-list p))))

  ;; multiply term
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ;; procedures used by mul-poly
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           ;; t1 * t2 is the highest term
           ;; t1 * L = t1 * t2 + (t1 * res_L)
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (=zero?-poly p)
    (=zero? (term-list p)))

  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))

  (put 'make-from-dense 'polynomial
       (lambda (variable dense-term-list)
         (tag (make-poly-from-dense variable dense-term-list))))

  (put 'make-from-sparse 'polynomial
       (lambda (variable sparse-term-list)
         (tag (make-poly-from-sparse variable sparse-term-list))))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-poly p)))

  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)

(define (make-polynomial-from-dense variable dense-term-list)
  ((get 'make-from-dense 'polynomial) variable dense-term-list))

(define (make-polynomial-from-sparse variable sparse-term-list)
  ((get 'make-from-sparse 'polynomial) variable sparse-term-list))

(install-polynomial-package)

(define p1 (make-polynomial-from-dense 'x '(1 2 3 4 5)))
(define p2 (make-polynomial-from-sparse 'x '((5 2) (3 5) (2 2) (0 1))))

(add p1 p2)

(add p2 p1)

(mul p1 p2)

(mul p2 p1)

(neg p1)

(neg p2)

(sub p1 p2)

(sub p2 p1)