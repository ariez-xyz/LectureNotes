#lang racket
(define (comp f g)
  (lambda (x) (f (g x))))

(define (twice f)
  (comp f f))

(define (square x)
  (* x x))

#| 2.
f: a function or some number or string.
   e.g. f = 5: will return 5
(f): a function that takes no arguments.
     e.g. f = (lambda () 5) will return 5
(f 3): a function that takes a number as argument.
       e.g. f = square will return 9
((f)): a function that takes no arguments and returns a function that also takes no arguments.
       e.g. f = (lambda () (lambda () 5)) will return 5
(((f)) 3): a function that takes no arguments and returns a function that also takes no arguments and returns a function that takes a number as argument.
           e.g. f = (lambda () (lambda () square)) yields 9
|#