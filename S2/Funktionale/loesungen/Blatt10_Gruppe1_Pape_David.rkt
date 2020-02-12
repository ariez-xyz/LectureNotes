#lang racket

; 1) Hydra
; A hydra is defined as a list of numbers, their value representing the level of the head
; The list is kept flat.
(define (make-hydra heads)
  (make-list heads heads))

; 2) Strikes
; Kills a hydra, returns number of strikes.
; Slow.
(define (kill hydra) 
  (define (kill-head hydra index)
    (define (kill-result n)
      (make-list (- n 1) (- n 1)))

    (flatten (list-update hydra index kill-result)))

  (if (empty? hydra)
    0
    (+ 1 (kill (kill-head hydra 0)))))

; 3) Min/max concurrent heads
; Technically, minimum is always 0. Taking this as minimum number of heads while still at the "root"
; Minimum is achieved by killing the heads in ascending order.
; Minimum is given by n(n+1)/2 where n is the maximum of the list minus 1.
; Works with degenerate hydras (i.e. hydras that aren't '(n n n n ... n)
(define (min-concurrent-heads hydra)
  ; this is maximum number of heads spawned by any head in the list
  (define max (- (argmax (lambda (x) x) hydra) 1))
  ; need to take care of special cases.
  (if (< 2 max) 
    1 
    (+ 1 (/ (* max (+ max 1)) 2))))

; Maximum number of concurrent heads is given by sum of factorials of (list elements - 1)
; Also works with degenerate hydras
(define (max-concurrent-heads hydra)
  (define (factorial n)
    (if (= n 0)
      1
      (* n (factorial (- n 1)))))
  ; For proper hydras (factorial (car hydra)) suffices im place of this
  (foldl (lambda (x y) (+ x y)) 0 (map (lambda (x) (factorial (- x 1))) hydra)))


(provide make-hydra)
(provide kill)
(provide min-concurrent-heads)
(provide max-concurrent-heads)
