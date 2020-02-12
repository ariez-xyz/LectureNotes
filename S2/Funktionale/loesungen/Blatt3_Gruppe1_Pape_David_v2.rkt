#lang racket
(define (square x)
  (* x x))
(define (cube x)
  (* (square x) x))
(define (inc x)
  (+ x 1))



;1.
(define (twice f)
  (comp f f))


;2.
;f: eine Funktion oder ein Datentyp, z.B. f = null
null ;---> leere Liste '()

;(f): Funktion, die keine Argumente nimmt, z.B. einfache Lambda-Funktion
((lambda () 42)) ;---> 

;(f 3): Funktion, die eine Zahl als Argument nimmt, z.B. square
(square 3) ;---> 9

;((f)): Higher Order Funktion, die eine Funktion zurueckgibt, beide nehmen kein Argument
(((lambda () (lambda () 1337)))) ;---> 1337

;(((f)) 3): Higher Order Funktion, die eine Higher Order Funktion zurueckgibt, die eine Zahl als Argument nimmt. Die Higher Order Funktionen nehmen kein Argument.
((((lambda () (lambda () square)))) 11) ;---> 121


;3.
(define (comp f g)
  (lambda (x) (f (g x))))

((comp inc cube) 2)
((comp cube inc) 2)


;4.
(define (complex x y)
  (λ (m) (m x y)))

(define (complex-real z)
  (z (λ (p q) p)))

(define (complex-imaginary z)
  (z (λ (p q) q)))

(define (add-complex a b)
  (complex ((+ (complex-real a) (complex-real b)) (+ (complex-imaginary a) (complex-imaginary b)))))

(define (print-complex a)
  (display (complex-real a))
  (display "+")
  (display (complex-imaginary a))
  (display "i"))


#|
5.
a) cdr car (cdr x2)
   mit 2x cdr erhalten wir ((5 7) null). Es scheint, dass Listen in Listen
   in einer Subliste verschachtelt werden. mit car erhalten wir (5 7), mit cdr dann schliesslich 7.

b) car x2
   zweimal car, um 7 aus der verschachtelten Liste zu holen

c) (car cdr) x6
   sei ls die Liste in Frage. dann ist (cdr ls) eine List der Form (ls' null), wo ls' (2 (3 ... )) ist.
   Mit (car (cdr ls)) erhalten wir erst ls'. Wir wiederholen dies, bis wir zur 7 gelangen
|#


;6.
(define (getFirst n xs)
  (define (recHelper n xs)
    (if (= n 0)
        (list)
        (cons (car xs) (recHelper (- n 1) (cdr xs)))))
  (recHelper n xs))

(getFirst 3 (list 1 2 3 4 5))