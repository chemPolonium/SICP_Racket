#lang racket

(define (make-account balance password)
  (define incorrect-times 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    (displayln "Calling the cops"))
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (begin
          (set! incorrect-times 0)
          (cond [(eq? m 'withdraw) withdraw]
                [(eq? m 'deposit) deposit]
                [else (error "Unknown request: MAKE-ACCOUNT"
                             m)]))
        (begin
          (set! incorrect-times (add1 incorrect-times))
          (if (= incorrect-times 7)
              (begin
                (call-the-cops)
                (lambda _ "You can do nothing until the cops come"))
              (lambda _ "Incorrect password")))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)