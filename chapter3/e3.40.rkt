#lang sicp

(define (p1 x1 x2)
  (* x1 x2))

(define (p2 x1 x2 x3)
  (* x1 x2 x3))

(define x 10)

;;; not serialized

(display "not serialized:")
(newline)

; (p1) -> p2a1 -> p2a2 -> p2a3 -> p2c  -> p2s
(p2 (p1 x x) (p1 x x) (p1 x x))

; p2a1 -> (p1) -> p2a2 -> p2a3 -> p2c  -> p2s
(p2 x (p1 x x) (p1 x x))

; p2a1 -> p2a2 -> (p1) -> p2a3 -> p2c  -> p2s
(p2 x x (p1 x x))

; p2a1 -> p2a2 -> p2a3 -> (p1) -> p2c  -> p2s
(p2 x x x)

; p1a1 -> p1a2 -> p1c  -> (p2) -> p1s
(p1 x x)

; p1a1 -> (p2) -> p1a2 -> p1c  -> p1s
(p1 x (p2 x x x))

; (p2) -> p1a1 -> p1a2 -> p1c  -> p1s
(p1 (p2 x x x) (p2 x x x))

;;; serialized

(display "serialized")
(newline)

; (p1) -> (p2)
(p2 (p1 x x) (p1 x x) (p1 x x))

; (p2) -> (p1)
(p1 (p2 x x x) (p2 x x x))
