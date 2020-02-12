#lang racket

(display "=================================================================\n")
;==============================================================================
; A1 - getFromIdx

(define (getFromIdx idx xs)
    (cond
      [(= idx 0) xs]
      [else (getFromIdx (sub1 idx) (cdr xs))]
    )
)
(provide getFromIdx)

(getFromIdx 2 '(0 1 2 3 4 5 6))

(display "=================================================================\n")
;==============================================================================
; A2 - isSame

(define (isSame xs ys)
  (cond
    [(not (= (length xs) (length ys))) #f]
    [(empty? xs) #t]
    [(not (= (car xs) (car ys))) #f]
    [else (isSame (cdr xs) (cdr ys))]
  )
)
(define (isSameFold xs ys)
  (if (not (= (length xs) (length ys)))
      #f
      (foldr (lambda (x y prev) (and prev (= x y))) #t xs ys)
  )
)
(define (isSameMap xs ys)
  (andmap = xs ys)
)
(provide isSame)
(provide isSameFold)
(provide isSameMap)

(isSame '(1 2) '(1))
(isSame '(1 2) '(1 2))
(isSame '(1 2 3 4) '(1 2 3 3))

(display "=================================================================\n")
;==============================================================================
; A3 - toCelsius

(define (toFahrenheit temps)
  (map (lambda (x) (+ 32 (* 1.8 x))) temps)
)
(define (toCelsius temps)
  (map (lambda (x) (/ (- x 32) 1.8)) temps)
)
(provide toFahrenheit)
(provide toCelsius)

(toCelsius '(-40 32 50))

(display "=================================================================\n")
;==============================================================================
; A4
(define (cube x) (* x x x))
; Comparison:
; http://www.wolframalpha.com/input/?i=sum+from+10+to+100+of+((x%5E3)*((x%2B1)+modulo+2))
(foldl (lambda (i sum) (if (= 0 (modulo i 2)) (+ sum (cube i)) sum)) 0 (range 10 101))

(display "=================================================================\n")
;==============================================================================
; A5 - foldr

(define (mylength xs)
  (foldr (lambda (lst init) (add1 init)) 0 xs)
)
(provide mylength)
