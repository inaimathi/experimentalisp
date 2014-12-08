#lang racket
(provide main)

(require compatibility/defmacro
	 "evaluator.rkt" "model.rkt")

(define-macro (def-prim name args body)
  `(bind! global-env ',name
	  (primitive ',args (lambda (env)
			      (let ,(map (lambda (arg)
                                           `(,arg (cdr (lookup env ',arg))))
					 args)
				,body)))))

(bind! 
 global-env 'the-env
 (primitive '() (lambda (env) env)))

(def-prim + (a b) (+ a b))
(def-prim - (a b) (- a b))
(def-prim / (a b) (/ a b))
(def-prim * (a b) (* a b))
(def-prim = (a b) (= a b))
(def-prim car (a) (car a))
(def-prim cdr (a) (cdr a))
(def-prim cons (a b) (cons a b))


(define (loop)
  (display "EXP>> ")
  (print (exp-eval (read) global-env))
  (displayln "")
  (loop))

(define (main)
  (displayln " Base env:")
  (display " ") (displayln global-env)
  (loop))

;; (require "experimentalisp.rkt") (main)
