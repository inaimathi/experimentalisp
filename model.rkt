#lang racket/base
(require racket/tcp)
(provide prim! primitive primitive?
	 procedure procedure?
	 fexpr fexpr?
	 arglist-of environment-of body-of

	 in-port in-port-pt in-port-label in-port?
	 out-port out-port-pt out-port-label out-port?

	 extend-env lookup bind! re-bind! arglist-env!

	 true? self-evaluating?)

(define (arglist-env! env arglist args)
  (if (and (null? arglist) (null? args))
      env
      (arglist-env! 
       (bind! env (car arglist) (car args)) 
       (cdr arglist) (cdr args))))

;;;;;;;;;; Callable forms
(struct primitive (env args body))
(struct procedure (env args body))
(struct fexpr (env args body))

(define (environment-of thing)
  (cond ((primitive? thing) (primitive-env thing))
	((procedure? thing) (procedure-env thing))
	((fexpr? thing) (fexpr-env thing))))

(define (arglist-of thing)
  (cond ((primitive? thing) (primitive-args thing))
	((procedure? thing) (procedure-args thing))
	((fexpr? thing) (fexpr-args thing))))

(define (body-of thing)
  (cond ((primitive? thing) (primitive-body thing))
	((procedure? thing) (procedure-body thing))
	((fexpr? thing) (fexpr-body thing))))

;;;;;;;;;; Ports
(struct in-port (pt label))
(struct out-port (pt label))

;;;;;;;;;; Environments
(define (extend-env env) (cons (make-hash) env))

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

(define-syntax prim!
  (syntax-rules ()
    [(prim env name (a ...) body)
     (bind! env 'name
	    (primitive 
	     env
	     '(a ...) 
	     (lambda (env)
	       (let ((a (cdr (lookup env 'a)))
		     ...)
		 body))))]))

;;;;;;;;;; Basics
(define (true? thing)
  (if (eq? 'false thing)
      #f
      #t))

(define (self-evaluating? thing)
  (or (eq? 'false thing)
      (eq? 'true thing)
      (null? thing)
      (string? thing)
      (number? thing)
      (char? thing)
      (port? thing)
      
      (primitive? thing)
      (procedure? thing)
      (fexpr? thing)))
