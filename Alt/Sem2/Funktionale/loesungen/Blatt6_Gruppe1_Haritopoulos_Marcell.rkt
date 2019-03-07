#lang racket

;==============================================================================
;========================== UTILITY PROCEDURES ================================
(define (streamPrint str)
  (displayln (stream->list str))
)
(provide streamPrint)

(define (streamPrintPart howMuch str)
  (define (consume str i)
    (cond
      [(= i howMuch) (display "…")]
      [else (display (~a (stream-first str) ", "))
            (consume (stream-rest str) (add1 i))
      ]
    )
  )
  (display "[")
  (consume str 0)
  (displayln "]")
)
(provide streamPrintPart)

; DEFINITION OF NATURAL NUMBERS
; Racket has (in-naturals) that is basically N_0
; To get N_0 \ {0} = N, we just need to remove the first element
(define naturals (stream-rest (in-naturals)))

(display "=================================================================\n")
;==============================================================================
; A1 - toCelsiusStream

; http://docs.racket-lang.org/reference/streams.html?q=stream#%28form._%28%28lib._racket%2Fstream..rkt%29._stream-cons%29%29
; rest-exp must return a stream itself!
; empty-stream is considered a stream by stream?
(define (toCelsius-mapless str)
  (define (step el)
    (/ (- el 32) 1.8)
  )
  (if (stream-empty? str)
      empty-stream
      (stream-cons (step (stream-first str)) (toCelsius (stream-rest str)))
  )
)
(define (toCelsius str)
  (stream-map (λ (x) (* (/ 5 9) (- x 32))) str)
)
(provide toCelsius)

(streamPrint (toCelsius (in-range 0 100)))


(display "=================================================================\n")
;==============================================================================
; A2 - infinite stream

; There are no empty infinite streams, but there are infinite streams with empty-stream elements,
; assume that
; Thus, it is no stream without element (stream-cons wouldn't allow such things),
; but an infinite stream with empty streams as elements
(define (infiniteEmptyStream)
  (define (stream-iter)
    (stream-cons empty-stream (stream-iter))
  )
  (stream-iter)
)
(provide infiniteEmptyStream)


(display "=================================================================\n")
;==============================================================================
; A3 - power of two stream
(define (powersOfTwo start)
  (unless (= (floor (log start 2)) (log start 2))
    (error "Given element " start " is not a power of two!")
  )

  (stream-cons start (powersOfTwo (* 2 start)))
)
(provide powersOfTwo)

(streamPrintPart 5 (powersOfTwo 64))


(display "=================================================================\n")
;==============================================================================
; A4 - stream-add
(define (stream-add s1 s2)
  (cond
    [(and (stream-empty? s1) (stream-empty? s2)) empty-stream]
    [(or  (stream-empty? s1) (stream-empty? s2)) (error "Both streams did not exhauste at the same time!")]
    [else (stream-cons (+ (stream-first s1) (stream-first s2)) (stream-add (stream-rest s1) (stream-rest s2)))]
  )
)
(provide stream-add)

(streamPrintPart 4 (stream-add naturals naturals))


(display "=================================================================\n")
;==============================================================================
; A5 - mySum
(define (mySum s [sum 0])
  (stream-cons (+ (stream-first s) sum) (mySum (stream-rest s) (+ (stream-first s) sum)))
)
(provide mySum)

(streamPrintPart 4 (mySum naturals))


(display "=================================================================\n")
;==============================================================================
; A6 - sieve
(define (primes)
  (define (prime-iter str)
    (define current-prime (stream-first str))
    (stream-cons current-prime (prime-iter (stream-filter (λ (el) (not (= 0 (modulo el current-prime)))) (stream-rest str))))
  )
  (prime-iter (stream-rest naturals)) ; 1 is no prime - remove it
)
(provide primes)

(streamPrintPart 20 (primes))
