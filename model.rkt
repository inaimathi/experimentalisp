#lang racket/base
(provide primitive primitive?
	 procedure procedure?
	 fexpr fexpr?
	 arglist-of body-of environment-of

	 global-env extend-env lookup bind! re-bind! arglist-env!

	 true? self-evaluating?)

;;;;;;;;;; Environments
(define global-env (list (make-hash)))
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
      (hash-set! (car env) name exp)))
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
	 (bind! env (car arglist) (car args)))))

;;;;;;;;;; Callable forms
(struct primitive (env args body))
(struct procedure (env args body))
(struct fexpr (env args body))

(define (arglist-of thing)
  (cond ((primitive? thing) (primitive-args thing))
	((procedure? thing) (procedure-args thing))
	((fexpr? thing) (fexpr-args thing))))
(define (body-of thing)
  (cond ((primitive? thing) (primitive-body thing))
	((procedure? thing) (procedure-body thing))
	((fexpr? thing) (fexpr-body thing))))
(define (environment-of thing)
  (cond ((primitive? thing) (primitive-env thing))
	((procedure? thing) (procedure-env thing))
	((fexpr? thing) (fexpr-env thing))))

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
