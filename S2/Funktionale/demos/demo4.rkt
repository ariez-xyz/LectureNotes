#lang racket

(define even (list 2 4 6 8))
(define odd (list 1 3 5 7))

;append (built-in)
(append even odd)

;myappend
(define (myappend xs ys)
  (if (null? xs)
      ys
      (cons (first xs) (myappend (rest xs) ys))))

;myappend
(myappend even odd)



;;4a
(define nil null)
(define mylist (list 1 2 3 4))

;mylast
(define (mylast xs)
  (if (null? (cdr xs))
      (car xs)
      (mylast (cdr xs))))

(mylast mylist)

;rev
(define (rev xs)
  (if (null? xs)
      null
      (append (rev (rest xs)) (list (first xs)))))


;;first, rest, last, take, drop
(first mylist)
(rest mylist)
(last mylist)
(take mylist 3)
(drop mylist 2)


;;4b

;map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;test
(map (λ(x) (* x x)) mylist)
(map sin  mylist)

;test map with other data structures: see the PS !


#|
;jfyi
;some further experiments (just ideas)
(define (insertNewAt xs x pos)
  (append (take xs pos) (list x) (drop xs pos)))   ;note: there are more efficient approaches e.g. with split-at

(insertNewAt mylist 9 2)
|#

;;;;;;;;;;;;;;;;;;;;
;;4c Example: even-fibs
;we assume we already have this: get the n-th fibonacci number
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;We want a list of all even fibonacci numbers Fib(k), where k is less than or equal a given integer n:
;Note: this is ugly. Don't look at this for too long ;-)      ... see below
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ([f (fib k)])
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))


;;;;;;;
;;even-fibs with conventional interfaces

(filter even? (map fib (range 0 10)))


;;;;;;;;;;;;
;;4d 
(range 10)
(range 10 20)
(range 20 40 2)
(range 20 10 -1)

(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x)) (list 1 2 3 4))
(map sin (list 0.0 (/ pi 2)))


;;4e
;;;;; useless (just to show you the principle used in myfilter)
(define (rebuild xs)
  (if (null? xs)
      null
      (cons (car xs) (rebuild (cdr xs)))))
;;;;;


(define (myfilter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (myfilter predicate (cdr sequence))))
        (else (myfilter predicate (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(accumulate + 0 (list 1 2 3 4 5)) ;15
(accumulate * 1 (list 1 2 3 4 5)) ;120
(accumulate cons null (list 1 2 3 4 5)) ;(1 2 3 4 5)

(foldr - 0 '(1 2))
(foldl - 0 '(1 2))
(foldr - 0 '(1 2 3 4))
(foldl - 0 '(1 2 3 4))

;4f
(foldr + 5000 (filter even? (map fib (range 0 10))))



;4g
;; some performance comparisons (with the built-in reverse)
(define lst (range 10000))
(time(void(rev lst)))
(time(void(reverse lst)))

;a tail-recursive 
(define (trrev xs)
  (define (r in out)
    (if (empty? in)
        out
        (r (cdr in) (append (list(car in)) out))))  ;(r (cdr in) (append (cons (car in) out)))))
  (r xs '()))

(trrev (list 1 2 3 4))

(time(void(trrev lst)))

