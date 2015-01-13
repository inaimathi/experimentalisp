#lang racket/base
(provide primitive primitive?
	 procedure procedure?
	 fexpr fexpr?
	 arglist-of environment-of body-of

	 global-env extend-env lookup bind! re-bind! arglist-env!

	 true? self-evaluating?)

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

(define (environment-of thing)
  (cond ((primitive? thing) global-env)
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
(struct port (host-port direction label))
(define (get-char! a-port) (read-char a-port))
(define (put-char! a-port char) (write-char char a-port))

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
	     '(a ...) (lambda (env)
			(let ((a (cdr (lookup env 'a)))
			      ...)
			  body))))]))

(define global-env 
  (let ((env (list (make-hash))))
    (bind! 
     env 'the-env
     (primitive '() (lambda (env) env)))
    
    (prim! env + (a b) (+ a b))
    (prim! env - (a b) (- a b))
    (prim! env / (a b) (/ a b))
    (prim! env * (a b) (* a b))
    (prim! env = (a b) (if (eq? a b) 'true 'false))

    (prim! env car (a) (car a))
    (prim! env cdr (a) (cdr a))
    (prim! env cons (a b) (cons a b))

    (prim! env print (thing) (begin (displayln thing) '()))

    (prim! env open-in-file! (fname) (port (open-input-file fname) 'in (cons 'file fname)))
    (prim! env open-out-file! (fname) (port (open-output-file fname) 'out (cons 'file fname)))
    ;; (prim! env connect (hostname port) (tcp-connect hostname port))

    (prim! env get-char! (a-port) (read-char (port-host-port a-port)))
    (prim! env put-char! (a-port c) (write-char c (host-port a-port)))
    env))

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
      (port? thing)
      
      (primitive? thing)
      (procedure? thing)
      (fexpr? thing)))
