#lang racket

(define (make-interval a b)
  (cons a b))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval0 x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval0 x
                 (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y)))))

(displayln "Exercise 2.7")
(displayln "defined at line 3")

(displayln "Exercise 2.8")
(define (sub-interval0 x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(displayln "Exercise 2.9")
(displayln "in comment")
;(lower-bound (+ a b c))
; =(+ (lower-bound a)
;     (lower-bound b)
;     (lower-bound c))
(mul-interval0 (make-interval 1 2)
               (make-interval 3 4))
(div-interval (make-interval 1 2)
              (make-interval 3 4))

(displayln "Exercise 2.10")
(define (div-interval-checkzero x y)
  (if (and (>= (upper-bound y) 0)
           (<= (lower-bound y) 0))
      (error "divide by zero\n")
      (div-interval x y)))
;will cause error:
;(div-interval-checkzero (make-interval 1 2) (make-interval -1 1))

(displayln "Exercise 2.11")
; # (a b) (c d)  lo  hi
; 1  + +   + +   ac  bd
; 2  + +   - +   bc  bd
; 3  + +   - -   bc  ad
; 4  - +   + +   ad  bd
; 5  - +   - +   chaos
; 6  - +   - -   bc  ac
; 7  - -   + +   ad  bc
; 8  - -   - +   ad  ac
; 9  - -   - -   bd  ac
(define (mul-interval x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (if (> a 0)
        ; case 1 2 3
        (if (> c 0)
            ; case 1
            (make-interval (* a c) (* b d))
            ; case 2 3
            (if (> d 0)
                ; case 2
                (make-interval (* b c) (* b d))
                ; case 3
                (make-interval (* b c) (* a d))))
        ; case 4 5 6 7 8 9
        (if (> b 0)
            ; case 4 5 6
            (if (> c 0)
                ; case 4
                (make-interval (* a d) (* b d))
                ; case 5 6
                (if (> d 0)
                    ; case 5
                    (make-interval (min (* b c) (* a d))
                                   (max (* a c) (* b d)))
                    ; case 6
                    (make-interval (* b c) (* a c))))
            ; case 7 8 9
            (if (> c 0)
                ; case 7
                (make-interval (* a d) (* b c))
                ; case 8 9
                (if (> d 0)
                    ; case 8
                    (make-interval (* a d) (* a c))
                    ; case 9
                    (make-interval (* b d) (* a c))))))))

(displayln "Exercise 2.13")
(define (make-interval-center-percent c pct)
  (let ((width (* c (/ pct 100))))
    (make-interval (- c width) (+ c width))))

(define (percent-tolerance i)
  (let ((center (/ (+ (upper-bound i) (lower-bound i)) 2))
        (width (- (upper-bound i) (lower-bound i))))
    (* (/ width center) 100)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(displayln "Exercise 2.14, 2.15")
; consider a simplest situation:
; a / a, which should be 1, but not 1
; the existence of same interval will
; introduce uncessary (wrong) uncertain.
; so one interval better only appears
; "one" time in a calculation.

(let ((r (make-interval 3 4)))
  (display (div-interval r r))
  (newline))

(let ((r1 (make-interval 3 4))
      (r2 (make-interval 5 6)))
  (display (par1 r1 r2))
  (newline)
  (display (par2 r1 r2))
  (newline))

(displayln "Exercise 2.16")
(display "A computer algebra system is needed, the program
need to have the ability to recognize the same interval.\n")