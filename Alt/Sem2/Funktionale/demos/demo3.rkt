#lang racket

;symbol
'a_symbol

;equality
(equal? 'a 'b)
(equal? 'a 'a)

;higher-order functions (returns a function)   (the code is quite useless)
(define (math id)
  (cond ((equal? id 's) sin)
        ((equal? id 'c) cos)
        #|((equal? id 'n) newton)|#))

((math 's) 1)   ;is then the same as (sin 1)



;some math ..... functions that return functions....
;derivative (see SICP 1.3.4)

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define mycos (deriv sin))

(cos pi)
(mycos pi)

(define (square x) (* x x))
(define f square)               ;f(x) = x^2
(define Df (deriv f))      ;f'(x) = 2x

(Df 5)   ;2x --> 10



;;;;; abstractions with data

;;; 3a: rational numbers

;not the most userfriendly approach:
(define (addRatNum n1 d1 n2 d2)
  (+ (* n1 d2)
     (* n2 d1)))

(define (addRatDenom n1 d1 n2 d2)
  (* d1 d2))

;for calculating 1/2 + 1/2 we would calculate
(addRatNum 1 2 1 2)   ;4 (numerator)
(addRatDenom 1 2 1 2) ;4 (denominator)  --> so the result is 4/4 .... 1




;; 3b: towards data abstraction

;pairs
(define p (cons 1 2))
(car p) ;1
(cdr p) ;2

;more pairs
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z)) ;1
(car (cdr z)) ;3


; 3c: rational numbers with pairs
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;test
(define onehalf (make-rat 1 2))
(numer onehalf)
(denom onehalf)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;test (see the docu for ~a if you're interested)
(define (print-rat r)
  (display (~a "Result: " (numer r) "/" (denom r) "\n")))
(print-rat (add-rat onehalf onehalf))

;improve make-rat
#|(define (make-rat n d) 
  (define g (gcd n d))
  (cons (/ n g) (/ d g)))
|#


; 3d: what is meant by data ?
;hmmmmm..... cons, car, cdr are built-in. are they difficult to implement?   no.  we can make cons, car, cdr ourselves.

(define (mycons x y)
  (define (dispatch m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "Argument not 0 or 1 -- in mycons" m)]))
  dispatch)

(define myPair (mycons 5 6))

myPair

;(myPair)    ;the expected number of arguments does not match the given number; expected: 1; given: 0

(myPair 0)
(myPair 1)

(define (mycar z) (z 0))
(define (mycdr z) (z 1))

;some more data structures (nothing new)
(define (make-person name age)
  (define (dispatch m)
    (cond [(eq? m 'name) name]
          [(eq? m 'age) age]
          [else (error "illegal property " m)]))
  dispatch)

(define (get property person)
  (person property))

;test
(define aPerson (make-person "Hans" 44))    ;in Java eg. Person aPerson = new Person("Hans", 44);
(get 'name aPerson)                         ;in Java eg. aPerson.getName();
(get 'age aPerson)                          ;in Java eg. aPerson.getAge();
;Note: racket also has classes, etc. - this is only to demonstrate what is actually required in a language



; 3e: cons, car, cdr with lambda   (nothing actually new; we just replace dispatch with an anonymous function)

(define (cons2 x y)
  (λ (m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "Argument not 0 or 1 -- in cons2" m)])))

;test
(define bla2 (cons2 5 6))
(mycar bla2)
(mycdr bla2)


;what about this?
(define (cons3 x y)
    (λ (m) (m x y)))

(define (car3 z)
    (z (λ (p q) p)))

(define (cdr3 z)
    (z (λ (p q) q)))

;test
(define bla3 (cons3 5 6))
(car3 bla3)
(cdr3 bla3)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;;; jfyi: built-in support: structure definitions

;when a structure gets defined
(define-struct rational (numer denom))

;the following operations will be created for us  (notice the naming convention):
;a constructor:
(make-rational 1 2)

(define r1 (make-rational 1 2)) ;creates a new rational

;and a selector for each field
(rational-numer r1)
(rational-denom r1)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;3f hierarchical data ;;;;;;;;;;;;;;;;;
(define a (cons 1 2))
(define b (cons (cons 1 2) (cons 3 4)))

(car a) ;1
(cdr a) ;2
(car (cdr b)) ;3

(define c (cons a b))

;;sequences
(cons 1 (cons 2 (cons 3 (cons 4 null) )))
;(cons 1 (cons 2 (cons 3 4))) ;this is not the same

;note: "null" was orinally "nil" in Lisp and Scheme
(define nil null)

nil
null
empty
'()
(list)


;shortcut: list
(list 1 2 3 4)
'(1 2 3)

(define mylist (list 1 2 3 4))
(define even (list 2 4 6 8))
(define odd (list 1 3 5 7))

(car mylist)
(cdr mylist)
(car (cdr mylist))
(car (cdr (cdr mylist)))


(first mylist)
(rest mylist)
(first (rest mylist))
(first (rest (rest mylist)))

(caddr mylist)

(displayln "-------------------")

;null?
(null? mylist)
(null? null)
(null? (list))
(null? (list null))

(empty? mylist)


;;;lists of different types

(list #t 4 'aSymbol "aString" 5)

(list (list 1 2) (list 3 4))

(list (list #t 2) (list #f 4) 'S)

;;;Examples [cons|list]
;(cons 1 2 3)  ;error: cons only takes two arguments

(cons 1 2) ;makes a pair '(1 . 2)

(cons (list 1) 2) ;makes a pair  '((1) . 2)

(cons 1 (cons 2 null)) ;makes a list (because of the empty list on the right)    '(1 2)

(cons 1 (list 2)) ;makes a list '(1 2)   ... so it takes the first argument, ie. 1, and makes it the first entry of the list

(cons (list 1) (list 2)) ;makes a list   '((1) 2)   ... again it takes the first argument, ie. a list, and makes it the first entry in the list

(list (list 1) (list 2))  ;makes a list of two elements (namely the original lists)    '((1) (2))

(append (list 1 2) (list 3 4))  ; '(1 2 3 4)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;3g list operations

;list-ref    get the n-th item (starting with 0)
(list-ref mylist 2)

(define (mylist-ref items n)
  (if (= n 0) (first items)
      (mylist-ref (rest items) (- n 1))))


(mylist-ref mylist 2)

(length mylist)

;get the length of the list
(define (mylength items)
  (if (null? items)
      0
      (+ 1 (mylength (rest items)))))

(mylength mylist)
