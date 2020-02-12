#lang racket
;1
[define [leaflist tree]
  [if [list? tree]
      [if [null? tree]
          tree
          [append [leaflist [first tree]] [leaflist [last tree]]]]
      [list tree]]]

[define x [list [list 1 [list 2 3]] [list 4 5]]]
[define tree [list x x]]
[leaflist tree]


;2
[define [nestingLevel xs]
  [if [and [list? xs] [not [null? xs]]]
      [+ 1 [apply max [map nestingLevel xs]]]
      0]]

[nestingLevel '[1 2 3 [list 4 5] [list 6 [list 7]]]]


;3
[define [zip op xs ys]
  [if [null? xs]
      xs
      [append [list [op [car xs] [car ys]]] [zip op [cdr xs] [cdr ys]]]]]

[zip list [list 1 2 3] [list 4 5 6]]


;4
[define [calc num summands]
  [define [minimize num summand iter]
    [if [>= [- num summand] 0]
        [minimize [- num summand] summand [+ iter 1]]
        [cons num iter]]]
  
  [define result [foldl [lambda [x y]
                          [define interim [minimize [car y] x 0]]
                          [append [list [car interim]] [cdr y] [list [cdr interim]]]]
                        [list num]
                        summands]]

  [if [= 0 [car result]]
      [cdr result]
      "no solution"]]

[calc 597 [list 200 100 50 20 10 5 2 1]]


;5
[define [count t]
  [accumulate-tree t [lambda [x] 1] + 0]]

[define [sum t]
  [accumulate-tree t [lambda [x] x] + 0]]

[define [fringe t]
  [accumulate-tree t [lambda [x] [list x]] append null]]


(define (accumulate-tree tree term op init)
    (cond ((null? tree) init)
          ((not (pair? tree)) (term tree))
          (else (op (accumulate-tree (car tree) term op init)
                    (accumulate-tree (cdr tree) term op init)))))

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(count t)
(sum t)
(fringe t)