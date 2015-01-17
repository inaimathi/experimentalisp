#lang racket/base
(provide main)

(require "evaluator.rkt" "model.rkt")

(define (loop)
  (display "EXP>> ")
  (let ((expression (read)))
    (display "   >> ")
    (displayln expression)
    (print (exp-eval expression global-env))
    (displayln "")) (displayln "")
  (loop))

(define (main)
  (displayln "experimentaLISP v0.00001")

  ;;;;; Initial display
  (displayln " Base env:")
  (display " ") (displayln global-env)

  (loop))
