#lang scheme

;(define (factorial n)
;  (if (= n 1)
;      1
;      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

;(define (fib n)
;  (cond ((= n 0) 0)
;        ((= n 1) 1)
;        (else (+ (fib (- n 1))
;                 (fib (- n 2))))))
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(fib 5)

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination
                           kinds-of-coins))
                       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amount 5))
(count-change 100)

;Exercise 1.11
(define (fib3 n)
  (define (next a b c)
    (+ (* 3 a)
       (* 2 b)
       c))
  (define (iter a b c nn)
    (if (= n nn)
        c
        (iter b c (next a b c) (+ 1 nn))))
  (if (< n 3)
      n
      (iter 0 1 2 2)))
(fib3 4)

(display "Exercise 1.12\n")
(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1))
         (pascal (- r 1) c))))
(pascal 4 3)

(display "Section 1.2.4\n")
(define (expt b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1)
              (* b product))))
  (iter n 1))
(expt 3 4)

(define (square x)
  (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(fast-expt 3 4)

(display "Exercise 1.16\n")
(define (fast-expt-tail b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))
(fast-expt-tail 2 10)

(display "Exercise 1.17\n")
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

(define (mult-by-add a b)
  (cond ((= b 0) 0)
        ((even? b) (mult-by-add (double a) (halve b)))
        (else (+ a (mult-by-add a (- b 1))))))
(mult-by-add 5 6)

(display "Exercise 1.18\n")
(define (mult-by-add-tail a b)
  (define (iter n a b)
    (cond ((= b 0) n)
          ((even? b) (iter n (double a) (halve b)))
          (else (iter (+ n a) a (- b 1)))))
  (iter 0 a b))
(mult-by-add-tail 5 6)

(display "Exercise 1.19\n")
(define (fast-fib n)
  (define (pp p q)
    (+ (square p) (square q)))
  (define (qq p q)
    (+ (square q)
       (double (* p q))))
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (pp p q)
                 (qq p q)
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))
(fast-fib 7)