#lang racket

(require (for-syntax syntax/parse
                     (only-in "lex.rkt" make-node))
         (prefix-in tr: typed/racket)
         math/array
         "syntax-classes.rkt"
         "lib.rkt")
(provide compile)

;each line was parsed to a single node, compile line-by-line
(define-syntax (compile stx)
  (syntax-parse stx
    [(_ (compiled ...) (ast asts ...) env)
     #`(compile (compiled ... #,(local-expand #`(compile-node ast #,(hasheq)) 'expression #f)) (asts ...) env)]
    [(_ (compiled ...+) () env)
     #'(tr:values compiled ...)]))

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
     #`(#%datum . #,(string->number (syntax->datum #'n)))]
    ;verb application
    #;[(_ (verb n #f #f #f) env) ;1st-class verb case, not implemented
     #`#,(local-expand #'(primitive-env-ref n) 'expression #f)]
    ;monadic verb
    [(_ (noun verb-app #f v:verb/j n:noun/j) env) ;1st compile the noun argument if not done
     #`(compile-node #,(make-node #'noun #'verb-app #f #'v (local-expand #'(compile-node n env) 'expression #f)) env)]
    [(_ (noun verb-app #f v:verb/j n) env) ;now compile the application with the 'noun type' 'erased'
     #`(#,(local-expand #`(primitive-env-ref v.symbol _) 'expression #f) n)]
    ;dyadic verb
    [(_ (noun verb-app n1:noun/j v:verb/j n2:noun/j) env)
     #`(compile-node #,(make-node #'noun #'verb-app
                                  (local-expand #`(compile-node n1 env) 'expression #f)
                                  #'v
                                  (local-expand #`(compile-node n2 env) 'expression #f)) env)]
    [(_ (noun verb-app n1 v:verb/j n2) env)
     #`(#,(local-expand #`(primitive-env-ref v.symbol _ _) 'expression #f) n1 n2)]))