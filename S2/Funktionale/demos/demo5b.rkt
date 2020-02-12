#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SICP chapter 3
 
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25) ;75
(withdraw 25) ;50
(withdraw 60) ;"Insufficient funds"
(withdraw 15) ;35


;;encapsulation
(define new-withdraw
  (let ((bal 100))
    (lambda (amount)
      (if (>= bal amount)
          (begin (set! bal (- bal amount))
                 bal)
          "Insufficient funds"))))

;;withdrawal processors
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;Make-withdraw can be used as follows to create two objects W1 and W2:

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50) ;50
(W2 70) ;30
(W2 40) ;"Insufficient funds"
(W1 40) ;10

;; a bankaccount as an object
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50) ;50
((acc 'withdraw) 60) ;"Insufficient funds"
((acc 'deposit) 40) ;90
((acc 'withdraw) 60) ;30


;;;;;;;;;;;;
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)
