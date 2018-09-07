#lang racket
;;3.1
(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ x acc))
    acc))

;;3.2
(define (make-monitored fun)
  (define count 0)
  
  (define (dispatch x)
    (cond ((number? x)
        (begin
          (set! count (+ 1 count))
          (fun x)))
          ((eq? x 'how-many-calls?)
           count)))
  dispatch)
    
;;3.3  3.4
(define (make-account balance pass)
  (define bad-times 0)
  (define (handle-bad-password am)
    (set! bad-times (+ 1 bad-times))
    (if (> bad-times 7)
        (error "call-the-cops")
        "Incorrect password"))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass- m)
    (if (eq? pass pass-)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"m)))
       handle-bad-password))
  dispatch)

