#lang racket

;;;;demo0 (a) expressions

5

(+ 4 6)

(sqrt 64)

(+ 4 3 2 1)

(+ 30)

(+)

(*)

+

'(+ 4 3)

(expt 2 1500)    ;note that e.g. Java's long data type (8byte) is max. ~ 2^63

;combinations
(+ (* 2 3) (+ 2 2))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))   ;57

;formatting (align operands) --> ignore linebreaks
(+ 
   (* 3 
      (+ (* 2 4) 
         (+ 3 5))) 
   (+ (- 10 7)
      6))

;;naming
(define radius 25)

radius

(define pi 3.14)

(* pi (* radius radius))

(define circ (* 2 pi radius))