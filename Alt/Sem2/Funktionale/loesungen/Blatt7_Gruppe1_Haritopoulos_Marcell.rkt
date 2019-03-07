#lang racket

; λ = U-03BB
; α = U-03B1
; β = U-03B2

(display "=================================================================\n")
;==============================================================================
; A1 - funcList

(define (funcList n)
  (define (curriedAdd idx)
    (define curried (curry + idx))
    (if (< idx n)
        (cons curried (curriedAdd (add1 idx)))
        null ; = '()
    )
  )
  (curriedAdd 0)
)
(provide funcList)

((list-ref (funcList 5) 0) 7)
((list-ref (funcList 5) 2) 7)


(display "=================================================================\n")
;==============================================================================
; A2 - beta reduction

#| Angabe: (λx.λy.λz. - (+ x y) z) 1 2 3
 |
 |  (λy.λz. - (+ 1 y) z) 2 3
 |  (λz. - (+ 1 2) z) 3
 |  (- (+ 1 2) 3)
 |  (- 3 3)
 |  0
 |#


(display "=================================================================\n")
;==============================================================================
; A3 - alpha conversion + normal form

#| Angabe: (λx.λy.(λx. + x x) y) 5 m
 |
 |  α-conversion:
 |      (λx.λy.(λz. + z z) y) 5 m
 |  b-reduction:
 |      (̀λy.(λz. + z z) y) m
 |      (λz. + z z) m
 |      (+ m m)
 |      Ergebnis hängt vom Wert der freien Variable m ab
 |#


(display "=================================================================\n")
;==============================================================================
; A4 - racket -> lambda expression

(define (g x y)
  (define (f x) (* x 2))
  (- x (f y))
)
(g 7 3)

#|  1. Schritt: (f x)
 |      (λx. * x 2)
 |  2. Schritt: α-construction
 |      (λz. * z 2)
 |  3. Schritt: (g x y)
 |      (λx. λy. - x (λz. * z 2) y) 7 3
 |#


(display "=================================================================\n")
;==============================================================================
; A5 - β-reduction

#|  (λx. λy. - x (λz. * z 2) y) 7 3
 |  (λy. - 7 (λz. * z 2) y) 3
 |  (- 7 (λz. * z 2) 3)
 |  (- 7 (* 3 2))
 |  (- 7 6)
 |  1
 |#

(displayln (~a "Das Ergebnis der β-reduction stimmt " (if (= 1 (g 7 3)) "" "nicht ") "mit dem Ergebnis des Aufrufes (g 7 3) überein"))
