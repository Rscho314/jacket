#lang racket

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse)
         math/array
         (only-in "syntax-classes.rkt"
                  execution-patterns))
(provide primitive-env-ref)

(define-syntax (primitive-env-ref stx)
  (syntax-parse stx
    ; no type-based dispatch in typed/racket
    ; so forced to dispatch on predicates at runtime
    [(_ ^ _)
     #'(tr:λ (y)
             (if (array? y)
                 (array-map tr:exp y)
                 (tr:exp y)))]
    [(_ ^ _ _)
     #'(tr:λ (x y)
             (if (array? x)
                 (array-map tr:expt x y)
                 (tr:expt x y)))]))