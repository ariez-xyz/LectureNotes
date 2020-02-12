#lang racket


(define (fac1) 1)
(define (fac2) (* 2 (fac1)))
(define (fac3) (* 3 (fac2)))
(define (fac4) (* 4 (fac3)))
(define (fac5) (* 5 (fac4)))

(fac5)



(define (fac n)
  (if (= n 1)
      1
      (* n (fac (sub1 n)))))
(fac 5)




;;note: we can trace the execution:
(require racket/trace)
;(trace myFunction)

#|
(require racket/trace)
(trace fac)
(trace fac1)
(trace fac2)
(trace fac3)
(trace fac4)
(trace fac5)
|#


;;Note: you can also use the debugger to see the stack growing


(define (print x)
  (display (~a "                 " x))
  (newline))

(define (count x)
  (cond ((= x 0) (print x))
        (else (print x)
              (count (- x 1))  ;X
              (print x) )))

(trace count)
(count 2)


;now take a look at this:

(define (fact n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; try this:
(trace fact)
(trace fact-iter)
(fact 5)
;;??? why is it different?


;don't care about the result:   void
(void 5)

;measure time
(time (void 4))

(untrace fac)
(untrace fact)
(untrace fact-iter)


;;;;; higher-order procedures
;;; procedures as arguments

;example-function (A): computes the sum of the integers from a through b
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

;test
(sum-integers 1 10)  ; 55

(sum-integers 1 4)  ;10


;helper function
(define (cube a) (* a a a))

;example-function (B): computes the sum of the cubes of the integers in the given range
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

;test
(sum-cubes 1 2)  ;9

;the general sum function
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;with the general sum function we can simplify sum-integers and sum-cubes
;but we need some (realy simple) helper functions first

(define (inc n) (+ n 1)) ;helper function to get the next number
(define (identity x) x) ;helper function

(define (sum-integers2 a b)
  (sum identity a inc b))

;test
(sum-integers2 1 10) ;55

(define (sum-cubes2 a b)
  (sum cube a inc b))

;test
(sum-cubes2 1 2)  ;9
;this is the same as
(sum cube 1 inc 2) ;9



;;; Lambda   λ

;are you tired of writing these tiny helper functions?
;"have you met ....." lambda (λ)

(lambda (x) (+ x 1))     ;this is a procedure


(λ(x) (+ x 1))           ;alternative


;inc expressed as λ:  instead of
;(sum cube 1 inc 2)   we write
(sum cube 1 (λ(x) (+ x 1)) 2)

;we can also avoid the named helper function cube and directly write
(sum (λ(x) (* x x x)) 1 (λ(x) (+ x 1)) 2)



;;;;;; one last example:
(define (square x) (* x x)) ;;helper
;1.34

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

;what happens here.... explain
; (f f)