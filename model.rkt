#lang racket/base
(provide primitive primitive?
	 procedure procedure?
	 fexpr fexpr?
	 make-partial partial? complete?
	 arglist-of environment-of body-of

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

(define (make-partial thing new-args)
  (letrec ((argl (arglist-of thing))
	   (rec (lambda (ks vs)
		  (cond ((and (null? ks) (null? vs)) '())
			((null? vs) ks)
			((null? ks) (error "Too many arguments"))
			(else (rec (cdr ks) (cdr vs)))))))
    (if (partial? thing)
	(partial (rec argl new-args) (append (partial-values thing) new-args) (partial-body thing))
	(partial (rec argl new-args) new-args thing))))

;;;;;;;;;; Callable forms
(struct primitive (args body))
(struct procedure (env args body))
(struct fexpr (env args body))

(struct partial (remaining-args values body))
(define (complete? thing)
  (eq? '() (partial-remaining-args thing)))

(define (environment-of thing)
  (cond ((primitive? thing) global-env)
	((procedure? thing) (procedure-env thing))
	((fexpr? thing) (fexpr-env thing))))

(define (arglist-of thing)
  (cond ((primitive? thing) (primitive-args thing))
	((procedure? thing) (procedure-args thing))
	((fexpr? thing) (fexpr-args thing))
	((partial? thing) (partial-remaining-args thing))))

(define (body-of thing)
  (cond ((primitive? thing) (primitive-body thing))
	((procedure? thing) (procedure-body thing))
	((fexpr? thing) (fexpr-body thing))))

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
      (char? thing)
      (primitive? thing)
      (procedure? thing)
      (fexpr? thing)
      (partial? thing)))
