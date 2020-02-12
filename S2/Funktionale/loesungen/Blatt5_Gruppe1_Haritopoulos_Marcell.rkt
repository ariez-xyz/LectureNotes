#lang racket
; λ = U-03BB
;(require racket/trace)
(define (trace x) (void))

(display "=================================================================\n")
;==============================================================================
; A1 - leaflist

(define (leaflist xs)
  (if (list? xs)
      (if (empty? xs)
        '()
        (append (leaflist (car xs)) (leaflist (cadr xs)))
      )
      (list xs)
  )
)
(provide leaflist)
(trace leaflist)

(define x '((1 2) (3 4)))
(leaflist (list x x))

(display "=================================================================\n")
;==============================================================================
; A2 - nestingLevel

(define (nestingLevel xs)
  (if (list? xs)
      (add1 (foldr max 0 (map nestingLevel xs)))
      0
  )
)
(provide nestingLevel)

(nestingLevel '(1 2 3))
(nestingLevel '((1 2) 3 4))
(nestingLevel '(1 (2 (3 4))))
(nestingLevel '((1 2) (3 4)))


(display "=================================================================\n")
;==============================================================================
; A3 - zip

(define (zip op xs ys)
  (unless (= (length xs) (length ys))
        (error "The lists must have the same cardinality")
  )

  (if (= 1 (length xs))
    (list (op (car xs) (car ys)) )
    (append (list (op (car xs) (car ys)) ) (zip op (cdr xs) (cdr ys)))
  )
)
(provide zip)

(zip + '(1 2 3) '(4 5 6))
(zip list '(1 2 3) '(4 5 6))



(display "=================================================================\n")
;==============================================================================
; A4 - calc

(define (calc-pair money bills)
  (when (< money 0)
    (error "The amount of money cannot be negative!"))
  (when (and (> money 0) (= 0 (length bills)))
    (error "Amount cannot be represented in given bills/coins! rest=" money))

  (if (= 0 (length bills))
      '()
      ;(cons (list (car bills) (floor (/ money (car bills)))) (calc-pair (modulo money (car bills)) (cdr bills)))
      (append (list (cons (car bills) (floor (/ money (car bills))))) (calc-pair (modulo money (car bills)) (cdr bills)))
  )
)
(define (pair-total billPairs)
  (foldr (λ (el prev) (+ prev (* (car el) (cdr el)))) 0 billPairs)
)
(define (calc money bills)
  (map cdr (calc-pair money bills))
)
(provide calc-pair)
(provide pair-total)
(provide calc)

(calc 432 '(100 10 5 2 1))
(with-handlers
  (
    [(λ (err) #t) (λ (err) (display (~a "Error occurred in (calc 432 '(100 10 5)): " (exn-message err) "\n")))]
  )
  (calc 432 '(100 10 5))
)

(define test-val 1705942)
(define test-result (pair-total (calc-pair test-val '(500 200 100 50 20 10 5 2 1))))
(unless (= test-val test-result)
  (error "Calc's result does not result to the same amount of money, either it has given us too much or kept some of it! result = " test-result)
)

(display "=================================================================\n")
;==============================================================================
; A5 - accumulate-tree

; accumulate-tree behaves like foldr/foldl in a way. It traverses the tree
; in-order and calls term on each leaf and combines the children of all nodes
; with op. Term could be used to pack leaves into a structure that is suitable
; to be an operator for op, be it a list/pair, a constant integer or the element
; itself (= identity function).
(define (accumulate-tree tree term op init)
  (cond
    [(null? tree) init]
    [(not (pair? tree)) (term tree)]
    [else (op (accumulate-tree (car tree) term op init) (accumulate-tree (cdr tree) term op init))]
  )
)
(trace accumulate-tree)
(provide accumulate-tree)

(define (count t) (accumulate-tree t (λ (x) 1)  + 0))
(define (sum t) (accumulate-tree t + + 0))
(define (fringe t) (accumulate-tree t list append '()))
(provide count)
(provide sum)
(provide fringe)

(define t '(1 (2 (3 4) 5) (6 7)))
(count t)
(sum t)
(fringe t)
