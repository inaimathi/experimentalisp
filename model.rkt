#lang racket/base
(provide primitive primitive?
	 procedure procedure?
	 fexpr fexpr? fexpr-partial?
	 make-partial partial? complete? collapse
	 arglist-of environment-of body-of

	 global-env make-env extend-env lookup bind! re-bind!

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

(define (make-partial thing new-args)
  (letrec ((argl (arglist-of thing))
	   (rec (lambda (ks vs)
		  (cond ((and (null? ks) (null? vs)) '())
			((null? vs) ks)
			((null? ks) (error "Too many arguments"))
			(else (rec (cdr ks) (cdr vs)))))))
    (if (partial? thing)
	(partial (rec argl new-args) (append (partial-values thing) new-args) (body-of thing))
	(partial (rec argl new-args) new-args thing))))

(define (collapse a-partial)
  (arglist-env!
   (extend-env (environment-of a-partial))
   (arglist-of (body-of a-partial))
   (partial-values a-partial)))

(define (arglist-env! env arglist args)
  (if (and (null? arglist) (null? args))
      env
      (arglist-env! 
       (bind! env (car arglist) (car args)) 
       (cdr arglist) (cdr args))))

;;;;;;;;;; Callable forms
(struct primitive (args body))
(struct procedure (env args body))
(struct fexpr (env args body))

(struct partial (remaining-args values body))
(define (complete? thing)
  (eq? '() (partial-remaining-args thing)))
(define (fexpr-partial? thing)
  (and (partial? thing) (fexpr? (partial-body thing))))

(define (environment-of thing)
  (cond ((primitive? thing) global-env)
	((procedure? thing) (procedure-env thing))
	((fexpr? thing) (fexpr-env thing))
	((partial? thing) (environment-of (body-of thing)))))

(define (arglist-of thing)
  (cond ((primitive? thing) (primitive-args thing))
	((procedure? thing) (procedure-args thing))
	((fexpr? thing) (fexpr-args thing))
	((partial? thing) (partial-remaining-args thing))))

(define (body-of thing)
  (cond ((primitive? thing) (primitive-body thing))
	((procedure? thing) (procedure-body thing))
	((fexpr? thing) (fexpr-body thing))
	((partial? thing) (partial-body thing))))

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
