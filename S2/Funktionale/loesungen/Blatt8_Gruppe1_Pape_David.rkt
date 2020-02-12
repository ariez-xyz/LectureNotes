#lang racket
;3 (mc-eval ((lambda (x) (x x)) (lambda (x) (x x))) null)

;5
[define [as-church n]
  [cond
    [[= 0 n] zero]
    [else [succ [as-church [- n 1]]]]
  ]
]