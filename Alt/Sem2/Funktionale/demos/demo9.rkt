#lang racket

;;thunks
(define th1 (thunk (define x 1) (printf "~a\n" x)))
(define th2 (λ() (define x 1) (printf "~a\n" x)))
(define th3 (λ() (define x 0) (printf "~a\n" (/ 10 x))))

(th1)
(th2)
;(th3)



;Closures (1)
(define (startAt x)
   (define (incrementBy y)
       (+ x y))
   incrementBy)

(define closure1 (startAt 1))
(define closure5 (startAt 5))

(closure1 9)
(closure5 9)


;;Closures (2)
(define (tankProcess pricePerLiter)
  (define (tankUp amountInLiter)
    (* pricePerLiter amountInLiter))
  
  tankUp)
 
(define price 1.0)
(define gas1 (tankProcess price)) ;;P1 starts the tank process
(set! price 1.5)                  ;;the price is increased
(define gas2 (tankProcess price)) ;;P2 starts the tank process

;;both pay at the same time (after the price was increased)
(gas1 100)
(gas2 100)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;Pattern matching (see e.g. http://docs.racket-lang.org/guide/match.html)

(define (pm mylist)
  (match mylist
    [(list a b a) (list a b)]
    [(list a b c) (list c b a)]))

(pm (list 1 2 1))
(pm '(1 (x y z) 1))



(match '(1 2 3)
    [(list _ _ a) a])


#|
;-------------- jfyi (begin) ------------------------------
(define (pm2 x)
  (match x
    [(list a b a) (list a b)]
    [(list a b c) (list c b a)]
    [(cons a b) (~a "CONS: " (car x) ", " (cdr x)) ]
    [(? symbol? a) "SYMBOL"]
    [(? boolean? b) "a boolean!"]
    [_ "I have no idea what that is"]))

(struct shoe (size color))
(struct hat (size style))

(define s (shoe 46 'black))

 (match s
   [(shoe size _) (~a "shoes with size " size)]
   [(hat _ _) "a hat"])

(match '(1 2 3 4 5)
  [(list x ...) (print x)])

(displayln "")

(match '(1 2 3 4 5)
  [(list x ... y) (begin (display x) (display "\n") (display y))])

(displayln "")

(match '(1 2 3 4 5)
  [(list 1 x ... 3 y ... z) y])

(match '(1 2 3 4)
  [(list-no-order 3 x 4 1) x])  ; 2

;http://www.jonathanturner.org/2011/09/fun-with-pattern-matching-in-racket.html

;;
(define ml (range 10))
(define-struct Person (name age))

(define mp (Person "Hans" 55))

(define (f xs)
  (match xs
    [(cons head tail) (~a head ":" tail)]
    [Person (displayln (~a "it is a person" " with name " (Person-name xs)))]))

(f ml)
(f mp)


;;match with regular expressions (regex).... TODO
;-------------- jfyi (end) ------------------------------
|#





;myappend (as we know it)
(define (myappendOld xs ys)
  (if (null? xs)
      ys
      (cons (first xs) (myappendOld (rest xs) ys))))


;myappend with pattern matching !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(define (myappend xs ys)
  (match xs
    ['() ys]
    [(cons xH xT) (cons xH (myappend xT ys))]))

(myappend (range 1 10) (range 10 20))





#|
;;jfyi (begin) //////////////////////////////////////

;;; lambda vs. thunk vs. delay
(define (answer-to-life)
 (begin (sleep 2) 42)) 

(define lambdaAnswer
 (lambda () (answer-to-life)))

(displayln "--")
(lambdaAnswer) ; sleeps 2 secs, then outputs 42
(lambdaAnswer) ; again sleeps 2 secs, then outputs 42

(define delayedAnswer
 (delay (answer-to-life)))

(displayln "--")
(force delayedAnswer) ; 2 second pause, then 42  -- caches result for further use
(force delayedAnswer) ; immediately provides result 42

(define thunkAnswer
 (thunk (answer-to-life)))

(displayln "--")
(force thunkAnswer) ; immediately provides result 42 (cached from above)
(force thunkAnswer) ; immediately provides result 42

;;jfyi (end) //////////////////////////////////////
|#
