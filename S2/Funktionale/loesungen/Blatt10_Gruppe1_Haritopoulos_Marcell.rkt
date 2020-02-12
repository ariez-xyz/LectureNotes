#lang racket

(define (sum lst)
    ;(foldr + 0 lst)
    (apply + lst)
    )
;(provide sum)

(define (memoize-list fn)
  (define memory (make-hash))
  (lambda (parm)
    (define key (sort parm <=))
    (unless (hash-has-key? memory key)
            (hash-set! memory key (fn key))
    )
    ;(displayln (~a key " returning " (hash-ref memory key)))
    (hash-ref memory key)
  )
)
;(provide memoize-list)

(define (memoize fn)
  (define memory (make-hash))
  (lambda (parm)
    (unless (hash-has-key? memory parm)
            (hash-set! memory parm (fn parm))
    )
    (hash-ref memory parm)
  )
)

(define (identity el) el)

;================================================

(define (makeHydra-recursive n)
  (define (hydra-head x)
    (if (= x n)
        '()
        (append (list n) (hydra-head (add1 x)))
    )
  )
  (hydra-head 0)
)
;(provide makeHydra)

(define (makeHydra-build n)
  (build-list n (lambda (el) n))
)

(define makeHydra makeHydra-build)

;================================================

(define (slayHead hyd [idx 0])
  (cond
    [(= idx 0)
        (if (= (sub1 (car hyd)) 0)
            (cdr hyd)
            (append (makeHydra (sub1 (car hyd))) (cdr hyd))
        )]
    [(= idx (length hyd)) '()]
    [else (append (list (car hyd)) (slayHead (cdr hyd) (sub1 idx)))]
  )
)
;(provide slayHead)

;================================================

(define (slayToDeath hyd)
  (if (= (length hyd) 0)
      0
      (add1 (slayToDeath (slayHead hyd)))
  )
)

(display "=================================================================\n")
;==============================================================================
; A3 - maxHeadCount

; = STEP 1 - Primitive recursive approach =
;
; This approach tries to find the maximum by trying all possible ways to slay
; the hydra. It looks at the sum of the current hydra's heads and generates
; a list of possible ways to slay one head of the hydra. Afterwards,
; the function feeds back the elements of the head list using recursion
; This works well for two-headed hydras, but takes too long afterwards

(define (listOfPossibleSlays hyd [idx 0])
  (if (= idx (length hyd))
      '()
      (append (list (slayHead hyd idx)) (listOfPossibleSlays hyd (add1 idx)))
  )
)

(define (maxHeadCount-recursive hyd)
  (define (foldMax lst currentMax)
    (max currentMax (maxHeadCount-recursive lst))
  )
  (foldr foldMax (sum hyd) (listOfPossibleSlays hyd))
)

; = STEP 2 - memoize =
;
; The results of maxHeadCount-recursive will be memoized.
; memoize-list has a small, but important optimization:
;   For the hydra, it is irrelevant if we change the heads'
;   ordering, the maximum amount of simultaneous heads will be the same.
;   For our hashmap, however, an unsorted key is a totally different thing
;   compared to a sorted key, even though they yield to the same result.
; This works good up until 4-headed hydras.
(set! maxHeadCount-recursive (memoize-list maxHeadCount-recursive))

; = STEP 3 - expansion =
;
; We do not need to look upon each possibility. As soon as we slay each head so
; that each stump has one or two heads, we already know the definite answer
; because this is the point where no other heads will spawn. when they are
; slayed.
; So the best way is to expand the heads recursively
; This works well up until 500 (with memoize)
(define (maxHeadCount-expand hyd)
  (define (mapCount el)
    (if (<= el 2)
        el
        (maxHeadCount-expand (slayHead (list el)))
    )
  )
  ;(displayln (~a "Expand " hyd))
  (sum (map mapCount hyd))
)

(set! maxHeadCount-expand (memoize-list maxHeadCount-expand))

; = STEP 4 - Hash map optimization =

; (1) has 1 max
; (2 2) has 4 max
; (3 3 3) -> (2 2) + (2 2) + (2 2) -> 4 + 4 + 4 -> 12 heads max
; (4 4 4 4) -> (3 3 3) + (3 3 3) + (3 3 3) + (3 3 3) -> 4*12 = 48 heads max
;
; (3 3 2) -> (2 2) + (2 2) + 2 -> 4 + 4 + 2 -> 10 heads max
; (4 4 2 3 1 2 3) -> (3 3 3) + (3 3 3) + (2) + (2 2) + (1) + (2) + (2 2)
;                 ->   12    +   12    +  2  +   4   +  1  +  2  +   4 = 37
;
; The size of the lists we used as keys previously might pose performance
; problems. However, we do not need to use lists as hash map keys - as
; illustrated above, we could just sum up the maximum heads per root.
; Thus, for 1 and 2 we just use this exact value as these are already the
; maximum amounts of heads for these specific values.
; For all other head counts, we still need to expand the heads. However,
; cutting one of those heads results in a list of (n-1) new heads that each
; spawn (n-1) heads when cut. We can calculate the maximum amount of heads by
; just having the amount of remaining spawning heads, thus a single integer.
; maxHeadsSingle is an optimization of maxHeadCount-opt. As written above, it
; does not need to operate on lists but utilizes raw integer operations that
; mimic the existance of our list, it "sums" up the max amount of (n-1) heads
; by multiplying it by n (because we have n heads that spawn (n-1) heads)
;
; This optimization yields the most drastic optimization and even works well
; up until 3000 (WITHOUT memoize!) and up until 75000
; (WITH memoize of maxHeadsSingle!), a rather gigantic beast.

(define (maxHeadsSingle heads)
  (if (<= heads 2)
      (* heads heads)
      (* heads (maxHeadsSingle (sub1 heads)))
  )
)
(define (maxHeadCount-opt hyd)
  (define (mapHeads el)
    (if (<= el 2)
        el
        (maxHeadsSingle(sub1 el))
    )
  )
  (sum (map mapHeads hyd))
)

(set! maxHeadsSingle (memoize maxHeadsSingle))

;(require racket/trace)
;(trace maxHeadsSingle)
;(trace maxHeadCount-opt)
;(trace maxHeadCount-expand)
