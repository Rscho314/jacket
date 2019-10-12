#lang racket

;; There is no parser bc J can't be parsed
;; Lexing useful bc ' and " have a scheme semantic meaning
(require "lex.rkt")

(provide read-syntax)

(define (read-syntax _ port)
  (datum->syntax #f `(module mod/j "expander.rkt" ,(lex/j port))))