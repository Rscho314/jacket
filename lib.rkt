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
    ;implementation for scalars/arrays
    ; -implement multimethods in macros (sanest but hardest way)
    ; -match at runtime
    ; -compiling verb nodes and simultaneously pattern matching on node name in 'compile-node' (complex & dirty without doing type inference)
    ; -implement full type inference (which we want to avoid)
    ; -compiling name nodes first and then pattern match on those to drive verb compilation (current attempt)
    ;  + issue: this requires much more branching in the compiling phase
    ;  + issue: cannot use case lambdas as this would require type-based dispatch
    ;verbs (so there is actually only 4 cases bc of scalar lifting)
    [(_ ^ scalar-arg)
     #'(tr:λ ([y tr:: tr:Number]) (tr:exp y))]
    [(_ ^ array-arg)
     #'(tr:λ ([y tr:: (Array tr:Number)]) (array-map tr:exp y))]
    [(_ ^ scalar-arg scalar-arg)
     #'(tr:λ ([x tr:: tr:Number] [y tr:: tr:Number]) (tr:expt x y))]
    [(_ ^ array-arg array-arg)
     #'(tr:λ ([x tr:: (Array tr:Number)] [y tr:: (Array tr:Number)]) (array-map tr:expt x y))]))