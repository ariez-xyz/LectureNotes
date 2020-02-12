#lang racket
;1
[define [toCelsius str]
  [stream-map [lambda [x] [* [- x 32] [/ 5 9]]] str]]

;2
[define [empty-str]
  [stream-cons [void] [empty-str]]] 

;3
[define [powersOf2Stream start]
  [stream-cons start [powersOf2Stream [* 2 start]]]]

[define po2s [powersOf2Stream 2]]

;4
[define [stream-add s1 s2]
  [stream-cons [+ [stream-first s1] [stream-first s2]] [stream-add [stream-rest s1] [stream-rest s2]]]]

;5
[define [my-sum s sum]
  [stream-cons [+ sum [stream-first s]] [my-sum [stream-rest s] [+ sum [stream-first s]]]]]

;6
[define [sieve s num max]
  [if [= num max]
      s
      [sieve [stream-filter [lambda [x] [> [remainder x num] 0]] s] [+ 1 num] max]]]