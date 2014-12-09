#lang racket/base
(provide primitive primitive? primitive-args primitive-body
	 procedure procedure? procedure-env procedure-args procedure-body
	 fexpr fexpr? fexpr-env fexpr-args fexpr-body

	 global-env make-env extend-env lookup bind! re-bind! arglist-env!

	 true? self-evaluating?)

;;;;;;;;;; Environments
(define (make-env) (list (make-hash)))
(define global-env (make-env))
(define (extend-env env)
  (cons (make-hash) env))
(define (lookup env exp)
  (cond ((null? env) '())
	((hash-has-key? (car env) exp)
	 (cons exp (hash-ref (car env) exp)))
	(else
	 (lookup (cdr env) exp))))
(define (bind! env name exp)
  (if (hash-has-key? (car env) name)
      (error (format "BIND: tried clobbering name '~a'...~%" name))
      (hash-set! (car env) name exp))
  env)
(define (re-bind! env name exp)
  (cond ((null? env) 
	 (error (format "RE-BIND: symbol '~a' is unassigned~%" name)))
	((hash-has-key? (car env) name)
	 (hash-set! (car env) name exp))
	(else
	 (re-bind! (cdr env) name exp)))
  env)

(define (arglist-env! env arglist args)
  (cond ((and (null? arglist) (null? args))
	 env)
	((null? arglist)
	 ;; too many args; error
	 (error (format "Too many arguments...~%"))
	 )
	((null? args)
	 ;; too few args; return a partial
	 (error (format "Not enough arguments...~%"))
	 )
	(else
	 (arglist-env! 
	  (bind! env (car arglist) (car args)) 
	  (cdr arglist) (cdr args))))
  env)

;;;;;;;;;; Callable forms
(struct primitive (args body))
(struct procedure (env args body))
(struct fexpr (env args body))

;;;;;;;;;; Basics
(define (true? thing)
  (if (eq? 'false thing)
      #f
      #t))

(define (self-evaluating? thing)
  (or (boolean? thing)
      (null? thing)
      (string? thing)
      (number? thing)
      (char? thing)))