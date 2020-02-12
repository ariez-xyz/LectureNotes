#lang racket

;David Pape 01634454

#|
Ich bin ein multiline comment
|#

(- (* 5 (+ 12 8)) (* 5 4))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fact 5)

#|

|#

(define (isEven m)
  (if (= (modulo m 2) 0)
      #t
      #f))

(isEven 201)