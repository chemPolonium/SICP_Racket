#lang racket

(displayln "2.5.2 Combining Data of Different Types")

(displayln "Coercion")

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

; defined later in section 2.5.2
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (error
;            "No methos for these types: APPLY-GENERIC"
;            (list op type-tags))))))

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
  'done)

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
  ;; for exercise 2.83
  (put 'numer '(rational)
       (lambda (x) (numer x)))
  (put 'denom '(rational)
       (lambda (x) (denom x)))
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

; new apply-generic with coercion support
; (define (apply-generic op . args)
;   (let* ([type-tags (map type-tag args)]
;          [proc (get op type-tags)])
;     (if proc
;         (apply proc (map contents args))
;         (if (= (length args) 2)
;             (let ([type1 (car type-tags)]
;                   [type2 (cadr type-tags)]
;                   [a1 (car args)]
;                   [a2 (cadr args)])
;               (let ([t1->t2 (get-coercion type1 type2)]
;                     [t2->t1 (get-coercion type2 type1)])
;                 (cond [t1->t2
;                        (apply-generic op (t1->t2 a1) a2)]
;                       [t2->t1
;                        (apply-generic op a1 (t2->t1 a2))]
;                       [else (error "No method for these types"
;                                    (list op type-tags))])))
;             (error "No method for these types"
;                    (list op type-tags))))))

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

(displayln "Hierarchies of types")

(displayln "Inadequacies of hierarchies")

(displayln "Exercise 2.81")

(require racket/trace)

(trace-define (scheme-number->scheme-number n) n)
(trace-define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(install-scheme-number-package)
(install-complex-package)

; nothing will happen because [proc (get op type-tags)]
; will apply the normal operation
(add (make-scheme-number 1) (make-scheme-number 1))

(exp 2 3)

; hash-ref: no value found for key
;   key: '(exp (complex complex))
;  [,bt for context]
; (exp (make-complex-from-real-imag 2 0)
;      (make-complex-from-real-imag 3 0))

; the apply-generic will not call the t1->t2 or t2->t1
; because it will not touch the coercion function at all
; if proc is not false

(displayln "Exercise 2.82")
; defined above

; (add 1 (make-complex-from-real-imag 2 3) 3)

; following will not work:
; t1->t3 and t2->t3 are true, but t1->t2 and t2->t1 are false

(displayln "Exercise 2.83")

; We should not use content because we do not know how to use the content.
; Only the type inner know how to use the contents,
; so we define a raise-table to log how to raise.
; The raise procedure will only use the out (high-level) procedure
; provided by the type.
; The type inner procedure should not know how to use other type's contructor.

(define (raise x)
  (let ([type (type-tag x)])
    ((get-coercion type (get-raise type)) x)))

; this will not work because real is scheme-number
(define (make-real x) x)

(define (numer x) (apply-generic 'numer x))

(define (denom x) (apply-generic 'denom x))

(define *raise-table* (make-hash))

(define (put-raise from to)
  (hash-set! *raise-table* from to))

(define (get-raise from)
  (hash-ref *raise-table* from))

(define (install-raise)
  (put-raise 'integer 'rational)
  (put-raise 'rational 'real)
  (put-raise 'real 'complex)
  (put-coercion 'integer 'rational (lambda (x) (make-rational x 1)))
  (put-coercion 'rational 'real (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put-coercion 'real 'complex (lambda (x) (make-complex-from-real-imag x 0))))

(install-raise)

(raise (make-rational 5 4))

(displayln "Exercise 2.84")

(define (level x)
  (match (type-tag x)
    ['integer 1]
    ['rational 2]
    ['real 3]
    ['complex 4]))

(define (raise-to x type)
  (if (equal? (type-tag x) type)
      x
      (raise-to (raise x) type)))

(define (raise-common args)
  (let* ([high-type (type-tag (argmax level args))]
         [raise-common-i (lambda (x) (raise-to x high-type))])
    (map raise-common-i args)))

(define (apply-generic-1 op . args)
  (let* ([raised-args (raise-common args)]
         [type-tags (map type-tag raised-args)]
         [proc (get op (type-tags))])
    (if proc
        (apply proc (map contents args))
        (error "No method for these types"
               (list op type-tags)))))

(displayln "Exercise 2.85")

(define *drop-table* (make-hash))

(define (put-drop from to)
  (hash-set! *drop-table* from to))

(define (get-drop from)
  (hash-ref *drop-table*))

(define (project x)
  (let ([type (type-tag x)])
    ((get-coercion type (get-drop type)) x)))

(define (equ? x y) (apply-generic 'equ? x y))

(define (drop x)
  (if (equ? (raise (project x)))
      (drop (project x))
      x))

(define (install-drop)
  (put-drop 'complex 'real)
  (put-drop 'real 'rational)
  (put-drop 'rational 'integer)
  (put-coercion 'complex 'real (lambda (x) (real-part x)))
  (put-coercion 'real 'rational (lambda (x) (make-rational (floor x) 1)))
  (put-coercion 'rational 'integer (lambda (x) (floor (/ (numer x) (denom x))))))

(define (apply-generic-2 op . args)
  (let* ([raised-args (raise-common args)]
         [type-tags (map type-tag raised-args)]
         [proc (get op (type-tags))])
    (if proc
        ;; we only need to drop the answer at the end
        (drop (apply proc (map contents args)))
        (error "No method for these types"
               (list op type-tags)))))

(displayln "Exercise 2.86")

; The complex is combined with ordinary numbers,
; rational numbers or other type of numbers, so the system no longer works.
; We need to make a new complex package.

; Following code is not tested.
; I gave up.
(define (square-1 x) (mul x x))

(define (cosine x) (cos (raise-to x 'real)))

(define (sine x) (sin (raise-to x 'real)))

(define (sqrt-1 x) (sqrt (raise-to x 'real)))

(define (install-rectangular-package-1)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

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

(define (install-polar-package-1)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt-1 (add (square x) (square y)))
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

(define (real-part-1 z) (apply-generic 'real-part z))

(define (imag-part-1 z) (apply-generic 'imag-part z))

(define (magnitude-1 z) (apply-generic 'magnitude z))

(define (angle-1 z) (apply-generic 'angle z))

(define (install-complex-package-1)
  (install-rectangular-package)
  (install-polar-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
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
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex z1 z2)))
  ;; from exercise 2.80
  (define (=zero?-complex z)
    (and (=zero? (real-part z))
         (=zero? (imag-part z))))
  (put '=zero? '(complex)
       (lambda (z) (=zero?-complex z)))
  'done)

(define (=zero? x) (apply-generic '=zero? x))

(define (make-complex-from-real-imag-1 x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang-1 r a)
  ((get 'make-from-mag-ang 'complex) r a))