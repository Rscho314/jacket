#lang racket

(require (for-syntax syntax/parse)
         (prefix-in tr: typed/racket)
         math/array
         "syntax-classes.rkt"
         "lib.rkt")
(provide compile)

;each line was parsed to a single node, compile line-by-line
(define-syntax (compile stx)
  (syntax-parse stx
    [(_ (compiled ...) (ast asts ...) env)
     #`(compile (values compiled ... #,(local-expand #`(compile-node ast #,(hasheq)) 'expression #f)) (asts ...) env)]
    [(_ compiled () env)
     #'compiled]))

;compile a line
(define-syntax (compile-node stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    ;nouns
    ;arrays
    [(_ (noun (n ...) #f #f #f) env)
     #`(array/syntax array tr:list unsafe-list->array #[#,@(map (compose string->number string) (syntax->datum #'(n ...)))])]
    ;scalars
    [(_ (noun n #f #f #f) env)
     #`(#%datum . #,(read (open-input-string (syntax->datum #'n))))]
    ;verb application
    #;[(_ (verb n #f #f #f) env) ;1st-class verb case, not implemented
     #`#,(local-expand #'(primitive-env-ref n) 'expression #f)]
    [(_ (noun verb-app #f v n:noun/j) env) ;preliminary compilation of noun node, fall-through on next 'compile-node'
     #`(compile-node (noun verb-app #f v #,(local-expand #'(compile-node n env) 'expression #f)) env)]
    [(_ (noun verb-app #f v:verb/j ((~literal #%datum) . d)) env) ;scalar monad case
     #`(#,(local-expand #`(primitive-env-ref v.symbol scalar-arg) 'expression #f)
        (#%datum . d))]
    [(_ (noun verb-app #f v:verb/j a) env) ;array monad case (brittle matching by elimination for now)
     #`(#,(local-expand #`(primitive-env-ref v.symbol array-arg) 'expression #f)
        a)]


    [(_ (noun verb-app n1:noun/j v n2:noun/j) env)
     #`(compile-node (noun verb-app
        #,(local-expand #'(compile-node n1 env) 'expression #f)
        v
        #,(local-expand #'(compile-node n2 env) 'expression #f)) env)]
    [(_ (noun verb-app ((~literal #%datum) . d1) v:verb/j ((~literal #%datum) . d2)) env) ;scalar scalar dyad case
     #`(#,(local-expand #`(primitive-env-ref v.symbol scalar-arg scalar-arg) 'expression #f)
        (#%datum . d1) (#%datum . d2))]
    [(_ (noun verb-app a v:verb/j ((~literal #%datum) . d)) env) ;array scalar dyad case (scalar lifted to array)
     #`(#,(local-expand #`(primitive-env-ref v.symbol array-arg array-arg) 'expression #f)
        a (array/syntax array tr:list unsafe-list->array #[#,@(reverse (syntax->list #'((#%datum . d))))]))]
    [(_ (noun verb-app ((~literal #%datum) . d) v:verb/j a) env) ;scalar array dyad case (same the other way round)
     #`(#,(local-expand #`(primitive-env-ref v.symbol array-arg array-arg) 'expression #f)
        (array/syntax array tr:list unsafe-list->array #[#,@(reverse (syntax->list #'((#%datum . d))))]) a)]
    [(_ (noun verb-app a1 v:verb/j a2) env) ;array array dyad case (array array case cannot be fused bc we don't know which to lift!)
     #`(#,(local-expand #`(primitive-env-ref v.symbol array-arg array-arg) 'expression #f)
        a1 a2)]))