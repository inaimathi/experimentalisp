#lang racket/base
(provide exp-eval exp-apply)

(require "model.rkt")

(define (exp-eval exp env)
  (cond ((self-evaluating? exp)
	 exp)
	((symbol? exp) 
	 (lookup env exp))
	((eq? 'fn (car exp))
	 (procedure env (cadr exp) (cddr exp)))
	((eq? 'fexpr (car exp))
	 (fexpr env (cadr exp) (cddr exp)))
	((eq? 'do (car exp))
	 (eval-sequence exp env))
	((eq? 'if (car exp))
	 (if (true? (exp-eval (cadr exp) env))
	     (exp-eval (caddr exp) env)
	     (exp-eval (cadddr exp) env)))
	((eq? 'def (car exp))
	 (eval-definition (cadr exp) (caddr exp) env))
	((eq? 'set! (car exp))
	 (eval-assignment (cadr exp) (caddr exp) env))
	((eq? 'quote (car exp))
	 (car (cdr exp)))
	((pair? exp)
	 (exp-apply (car exp) (cdr exp) env))
	(else
	 (error (format "EVAL: unknown form '~a'" exp)))))

(define (eval-sequence seq env)
  (cond ((null? seq) '())
	((null? (cdr seq))
	 (exp-eval (car seq) env))
	(else
	 (exp-eval (car seq) env)
	 (eval-sequence (cdr seq) env))))

(define (eval-args args env)
  (map (lambda (exp) (exp-eval exp env)) args))

(define (eval-definition name exp env)
  (if (symbol? name)
      (bind! env name (exp-eval exp env))
      (error (format "EVAL-DEFINIION: tried binding non-symbl '~a'" name))))

(define (eval-assignment name exp env)
  (if (symbol? name)
      (re-bind! env name (exp-eval exp env))
      (error (format "EVAL-ASSIGNMENT: tried assigning to non-symbol '~a'" name))))

(define (exp-apply op args env)
  (let ((fn (exp-eval op env)))
    (cond ((primitive? fn)
	   ((body-of fn)
	    (arglist-env! 
	     (extend-env env)
	     (arglist-of fn)
	     (eval-args args env))))
	  ((procedure? fn)
	   (eval-sequence (body-of fn)
			  (arglist-env!
			   (extend-env (environment-of fn))
			   (arglist-of fn)
			   (eval-args args env))))
	  ((fexpr? fn)
	   (exp-eval (eval-sequence
		      (body-of fn) 
		      (arglist-env!
		       (extend-env (environment-of fn))
		       (arglist-of fn)
		       args))
		     env)))))
