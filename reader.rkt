#lang racket

(require "lex.rkt")

(provide read-syntax)

(define (read-syntax _ port)
  (datum->syntax #f `(module mod/j "expander.rkt" ,(lex/j port))))