#lang racket

(define (ripple-carry-adder a-list b-list s-list c)
  (let ([c-list (map (lambda (x) (make-wire)) (cdr a-list))]
        [c-0 (make-wire)])
    (map full-adder
         a-list
         b-list
         (append c-list (list c-0))
         s-list
         (cons c c-list))
    'ok))

(define (half-adder a b s c)
  (let ([d (make-wire)] [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ([s (make-wire)] [c1 (make-wire)] [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Invalid signal" s)]))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value
           (logical-and (get-signal a1) (get-signal a2))])
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond [(and (= s1 1) (= s2 1)) 1]
        [(and (= s1 0) (= s2 0)) 0]
        [(and (= s1 1) (= s2 0)) 0]
        [(and (= s1 0) (= s2 1)) 0]
        [else (error "Invalid signal" s1 s2)]))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value
           (logical-or (get-signal a1) (get-signal a2))])
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond [(and (= s1 1) (= s2 1)) 1]
        [(and (= s1 0) (= s2 0)) 0]
        [(and (= s1 1) (= s2 0)) 1]
        [(and (= s1 0) (= s2 1)) 1]
        [else (error "Invalid signal" s1 s2)]))

(define (make-wire)
  (let ([signal-value 0]
        [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-value]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-procedure!]
            [else (error "Unknown operation: WIRE" m)]))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define after-delay 1)
(define inverter-delay 1)
(define and-gate-delay 1)
(define or-gate-delay 1)