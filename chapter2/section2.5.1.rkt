#lang racket

(displayln "2.5.1 Generic Arithmetic Operations")

(define *table* (make-hash))

; (define (attach-tag type-tag contents)
;   (cons type-tag contents))

; (define (type-tag datum)
;   (if (pair? datum)
;       (car datum)
;       (error "Bad tagged datum: TYPE-TAG" datum)))

; (define (contents datum)
;   (if (pair? datum)
;       (cdr datum)
;       (error "Bad tagged datum: CONTENTS" datum)))

; from exercise 2.78
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No methos for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (put func type value)
  (hash-set! *table* (list func type) value))

(define (get func type)
  (hash-ref *table* (list func type)))

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

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
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

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
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

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
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(displayln "Exercise 2.77")
(install-complex-package)
; (real-part '(complex rectangular 3 . 4))
; will get 'real-part 'complex, which is not defined
; after defined, (get '(real-part complex)) will be real-part
; then it will be (real-part '(rectangular 3 . 4))
; (get '(real-part rectangular)) is defined
(magnitude (make-complex-from-real-imag 3 4))

(displayln "Exercise 2.78")
; (define (attach-tag type-tag contents)
;   (if (number? contents)
;       contents
;       (cons type-tag contents)))

; (define (type-tag datum)
;   (cond [(number? datum) 'scheme-number]
;         [(pair? datum) (car datum)]
;         [else (error "Bad tagged datum: TYPE-TAG" datum)]))

; (define (contents datum)
;   (cond [(number? datum) datum]
;         [(pair? datum) (cdr datum)]
;         [error "Bad tagged datum: CONTENTS" datum]))

(add 1 2)

(displayln "Exercise 2.79")
; equ? added in "install-package"s

(define (equ? x y) (apply-generic 'equ? x y))

(equ? (make-complex-from-real-imag 1 0)
      (make-complex-from-mag-ang 1 0))

(equ? (make-complex-from-real-imag 3 4)
      (make-complex-from-real-imag 3 4))

(equ? (make-complex-from-real-imag 3 4)
      (make-complex-from-real-imag 3 5))

(equ? (make-rational 3 4)
      (make-rational -3 -4))

(equ? (make-rational 3 4)
      (make-rational -3 -5))

(equ? 1 1)

(equ? 1 2)

(displayln "Exereise 2.80")

(define (=zero? x) (apply-generic '=zero? x))

(=zero? 1)

(=zero? 0)

(=zero? (make-rational 1 1))

(=zero? (make-rational 0 1))

(=zero? (make-complex-from-real-imag 1 0))

(=zero? (make-complex-from-real-imag 0 0))
