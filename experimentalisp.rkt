#lang racket/base
(provide main)

(require "evaluator.rkt" "model.rkt")

(define (loop)
  (display "EXP>> ")
  (print (exp-eval (read) global-env))
  (displayln "") (displayln "")
  (loop))

(define (main)
  (displayln "experimentalisp 0.001")

  ;;;;; Initial display
  (displayln " Base env:")
  (display " ") (displayln global-env)

  (loop))
