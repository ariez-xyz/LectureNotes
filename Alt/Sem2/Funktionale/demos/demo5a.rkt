#lang racket

;;;;;; some more operations on lists

(define mySmallList (range 5))
(define myList (range 10000))   ;10k
(define myBigList (range 100000))   ;100k

(define (reverseA xs)  ; O(n^2)
  (if (null? xs)
      null
      (append (reverseA (cdr xs)) (list (car xs)))))


(define (reverseB xs [accum empty])   ;O(n) and tail-rec (and init-param)
    (if (empty? xs)
        accum
        (reverseB (cdr xs) (cons (car xs) accum))))

;(collect-garbage)
(time (void (reverseA myList))) ;1s-2s
;(collect-garbage)
(time (void (reverseB myList))) ;0s


;;; insertion sort
(define (insert x xs)
  (if (null? xs) 
      (list x)
      (if (< x (car xs))
          (cons x xs)
          (cons (car xs)(insert x (cdr xs))))))

(define (isort xs)
  (if (null? xs)
      null
      (insert(car xs) (isort(cdr xs)))))


;;;;;
;;init: all but last. e.g., (init '(1 2 3 4)) --> '(1 2 3)
(define (init xs)
  (cond [(null? xs) (error "empty list")]
        [(null? (cdr xs)) null]
        [else (cons (car xs) (init (cdr xs)))]))





;;;;5a-1 pair?, list? ;;;;;;;;;;;;;;;;;

;;pair?
(pair? (list 1))
(pair? (list 1 2))
(pair? (list 1 2 3))
(pair? (cons 1 2))
(pair? (cons 1 (cons 2 3)))
(displayln "--")
(pair? '())
(pair? 1)
(pair? "ein string")

(displayln "--------")

;;list?
(list? '())
(list? '(1 2))
(list? (cons 1 (cons 2 '())))
(displayln "--")
(list? (cons 1 2))
(list? 1)


;;;;;;;;;Hierarchical Structures;;;;;;

(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (leave? x)
  (not (pair? x)))

(define (count-leaves x)
  (cond [(null? x) 0]
        [(leave? x) 1]
        [else (+ (count-leaves (first x)) (count-leaves (rest x)))]))



;; Reverse for trees


#|
(define y (list( list (list 'a 'b) 1 2 (list 'c 'd)) (list 3 4) (list 5 6)))

(define (deep-reverse items)
  (cond ((null? items) null)
        ((pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items)))))
        (else
         (append (deep-reverse (cdr items))
                 (list (car items))))))

(define (deep-reverse2 items)
  (cond ((null? items) null)
        ((not (pair? items)) items)
        (else (append (deep-reverse2 (cdr items))
                 (list (deep-reverse2 (car items)))))))
|#


(define (deeprev l)
  (if (list? l)
      (reverse (map deeprev l))
      l))

(define y (list '(1 2) '(3 4) 5 '(6 7 8)))
(deeprev y)


#|
; scale for list (we already had this)
(define (scale lst factor)
  (map (λ (x) (* x factor)) lst))

(scale (list 1 2 3) 10)
|#

; scale for trees
(define (scale-tree tree factor)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(scale-tree t 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


