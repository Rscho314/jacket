#lang racket

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse
                     (only-in "lex.rkt" make-node))
         math/array
         "syntax-classes.rkt")

(provide (rename-out [module-begin/j #%module-begin]))

;parses & compiles
(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (())) #'(tr:#%module-begin)]
    [(_ prg) #`(tr:#%module-begin
                (compile () #,(local-expand #`(make-ast () prg #,(hasheq)) 'expression #f) #,(hasheq)))]))

;only parses (for debugging)
#;(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (())) #'(tr:#%module-begin)]
    [(_ prg) #`(tr:#%module-begin
                (make-ast () prg #,(hasheq)))]))

;make one AST per line
(define-syntax (make-ast stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    [(_ (line-trees ...) (line lines ...) env)
     #`(make-ast (#,(local-expand #`(make-line-ast line #,(hasheq)) 'expression #f) line-trees ... ) (lines ...) env)]
    [(_ trees () env)
     #'trees]))

(define-syntax (make-line-ast stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    ;node is (part-of-speech name left middle right), where middle is the operator and left/right are left/right arguments
    ;reference to an identifier
    #;[(_ (n:name/j r ...) env)
     #`(make-line-ast (#,(make-node
                          (hash-ref (syntax-e #'env) (syntax-e #'n.symbol))
                          #'reference
                          #'()
                          #'n
                          #'()) (~? r) ...) env)] ; the env returns a list of parts of speech
    ;termination
    [(_ (cavn:cavn/j) env)
     #'cavn]
    ;monad 1
    [(_ (n:noun/j v1:verb/j v2:verb/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #'(make-line-ast (#,(make-node #'noun #'verb-app #f #'v1 #'n) v2 (~? next) (~? r) ...) env)]
    ;dyad 2
    [(_ (n1:noun/j v:verb/j n2:noun/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node #'noun #'verb-app #'n2 #'v #'n1) (~? next) (~? r) ...) env)]
    ;conjunction 4 (can yield any part of speech, for now only verb)
    [(_ (vn1:vn/j c:conjunction/j vn2:vn/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node #'verb #'conjunction-app #'vn2 #'c #'vn1) (~? next) (~? r) ...) env)]
    ;fork 5 (can yield any part of speech?)
    [(_ (v1:verb/j v2:verb/j vn:vn/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node #'verb #'fork #'vn #'v2 #'v1) (~? next) (~? r) ...) env)]
    ;monad 0 brittle implementation using noun elimination for p&e 'anything' since a sequence of noun means an array
    [(_ ((~optional o:cav/j) n:noun/j v:verb/j (~optional (~or* next:copula/j next:lparen)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node #'noun #'verb-app #f #'v #'n) (~? next) (~? r) ...) env)]
    ;adverb 3 (can yield any part of speech, for now only verb)
    [(_ ((~optional o) a:adverb/j (~or* vn:verb/j vn:noun/j) (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node #'verb #'adverb-app #'vn #'a #f) (~? next) (~? r) ...) env)]
    ;hook/adverb 6
    [(_ ((~optional o) cavn1:cavn/j cavn2:cavn/j (~optional (~or* next:copula/j next:lparen)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node #'verb #'hook #f #'cavn2 #'cavn1) (~? next) (~? r) ...) env)]
    ;is 7
    [(_ ((~optional o) v:cavn/j _:copula/j n:name/j r ...) env)
     #`(make-line-ast ((~? o) #,(make-node #f #|unit type for assignment|# #'assignment #f #'n #'v) (~? r) ...) env)]
    ;parentheses 8
    [(_ ((~optional o) rparen cavn:cavn/j lparen r ...) env)
     #`(make-line-ast ((~? o) #,(local-expand #'(make-line-ast cavn #,(hasheq)) 'expression #f) (~? r) ...) env)]))

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