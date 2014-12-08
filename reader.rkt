#lang racket/base
(provide exp-read)

(require "model.rkt")

(define (exp-read in))
(define (read-s-exp in rest))
(define (read-dashed in rest))
(define (read-string in rest))
(define (read-number in rest))
(define (read-quoted in rest))
(define (read-symbol in rest))

;; open-input-string
;; peek-char
