#lang racket
(require math/number-theory)

(display "Section 1.3.1\n")
(define (sum-integers-old a b)
  (if (> a b)
      0
      (+ a (sum-integers-old (+ a 1) b))))

(define (cube x)
  (* x x x))
(define (sum-cubes-old a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes-old (+ a 1) b))))

(define (pi-sum-old a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum-old (+ a 4) b))))

(define (sum-recurse term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recurse term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-recurse-cubes a b)
  (sum-recurse cube a inc b))
(sum-recurse-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
  (sum-recurse identity a inc b))
(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-recurse pi-term a pi-next b))
(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum-recurse f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(display "Exercise 1.29\n")
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (coefficient k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (simpson-term k)
    (* (coefficient k)
       (f (+ a (* k h)))))
  (* (sum-recurse simpson-term 0 inc n)
     (/ h 3)))
(simpson-integral cube 0 1.0 100)

(display "Exercise 1.30\n")
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)

(display "Exercise 1.31\n")
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (term a)))))
  (iter a 1.0))
(define (factorial n)
  (product identity 1 inc n))
(define (product-pi n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2) (+ k 1))
        (/ (+ k 1) (+ k 2))))
  (* 4
     (product term 1 inc n)))
(define (product-recurse term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (product-recurse term (next a) next b))))
(define (factorial-recurse n)
  (product-recurse identity 1 inc n))
(factorial 4)
(factorial-recurse 4)
(product-pi 50)

(display "Exercise 1.32\n")
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))
(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))
(define (product-accumulate term a next b)
  (accumulate * 1.0 term a next b))
(define (sum-accumulate-integers a b)
  (sum-accumulate identity a inc b))
(define (factorial-accumulate n)
  (product-accumulate identity 1 inc n))
(sum-accumulate-integers 1 10)
(factorial-accumulate 5)

(display "Exercise 1.33\n")
(define (square x)
  (* x x))
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))
(define (sum-prime-square a b)
  (define (next x)
    (if (< x 3)
        (+ x 1)
        (+ x 2)))
  (filtered-accumulate + 0 square a next b prime?))
(define (relative-prime? n x)
  (= (gcd n x) 1))
(define (product-relative-prime n)
  (define (relative-prime-to-n? x)
    (relative-prime? n x))
  (filtered-accumulate * 1 identity 1 inc n relative-prime-to-n?))
(sum-prime-square 1 5)
(product-relative-prime 10)