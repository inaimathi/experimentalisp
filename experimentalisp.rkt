#lang racket/base
(provide main)

(require "evaluator.rkt" "model.rkt")

(define-syntax prim
  (syntax-rules ()
    [(prim name (a ...) body)
     (bind! global-env 'name
	    (primitive 
	     '(a ...) (lambda (env)
			(let ((a (cdr (lookup env 'a)))
			      ...)
			  body))))]))

(define (loop)
  (display "EXP>> ")
  (print (exp-eval (read) global-env))
  (displayln "") (displayln "")
  (loop))

(define (main)
  (displayln "experimentalisp 0.001")

  ;;;;; Basic primitives
  (bind! 
   global-env 'the-env
   (primitive '() (lambda (env) env)))
  
  (prim + (a b) (+ a b))
  (prim - (a b) (- a b))
  (prim / (a b) (/ a b))
  (prim * (a b) (* a b))
  (prim = (a b) (if (eq? a b) 'true 'false))
  (prim car (a) (car a))
  (prim cdr (a) (cdr a))
  (prim cons (a b) (cons a b))
  (prim print (thing) (begin (displayln thing) '()))

  ;;;;; Initial display
  (displayln " Base env:")
  (display " ") (displayln global-env)

  (loop))

;; (require "experimentalisp.rkt") (main)
