#lang racket

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse)
         math/array
         (only-in "syntax-classes.rkt"
                  execution-patterns))
(provide primitive-env-ref)

(define-syntax (primitive-env-ref stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    [(_ ^ Monadic Scalar)
     #'tr:exp]
    #;[(_ ^ _)
     #'(tr:λ (y)
             (if (array? y)
                 (array-map tr:exp y)
                 (tr:exp y)))]
    #;[(_ ^ _ _)
     #'(tr:λ (x y)
             (if (array? x)
                 (array-map tr:expt x y)
                 (tr:expt x y)))]))