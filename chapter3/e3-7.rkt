#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond [(eq? m 'withdraw) withdraw]
              [(eq? m 'deposit) deposit]
              [else (error "Unknown request: MAKE-ACCOUNT"
                           m)])
        (lambda _ "Incorrect password")))
  dispatch)

(define (make-joint account password new-password)
  (define (dispatch pwd m)
    (if (eq? pwd new-password)
        (account password m)
        (lambda _ "Incorrect password for new account")))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 20)

((peter-acc 'open-sesame 'withdraw) 10)