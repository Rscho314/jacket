#lang racket

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse)
         math/array
         "syntax-classes.rkt")

(provide (rename-out [module-begin/j #%module-begin]))

;parses & compiles
(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (())) #'(tr:#%module-begin)] ; empty program
    [(_ prg) #`(tr:#%module-begin
                (compile () #,(local-expand #`(make-ast () prg #,(hasheq)) 'expression #f) #,(hasheq)))])) ;top-level env

;only parses (for debugging)
#;(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (())) #'(tr:#%module-begin)] ; empty program
    [(_ prg) #`(tr:#%module-begin
                (make-ast () prg #,(hasheq)))])) ;top-level env

(define-syntax (make-ast stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    [(_ (line-trees ...) (line lines ...) env)
     #`(make-ast (#,(local-expand #`(make-line-ast line #,(hasheq 'a '(adverb verb noun))) 'expression #f) line-trees ... ) (lines ...) env)] ; line-level env
    [(_ trees () env)
     #'trees])) ; termination condition yields a tree per line

(define-syntax (make-line-ast stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    #|The type of some names is a union-type, so on simple type inference we get a
      list representing the type from the env, which is then matched against the
      syntax-classes to drive parsing (e.g. :verb/j matches a list containing 'verb, etc.).
      We'll use that to generate parse trees corresponding to all possible cases|#
    ;node is ((part-of-speech) name left middle right), where middle is the operator and left/right are left/right arguments
    ;reference to an identifier
    [(_ (n:name/j r ...) env)
     #`(make-line-ast (#,(make-node
                          (hash-ref (syntax-e #'env) (syntax-e #'n.symbol))
                          #'reference
                          #'()
                          #'n.node
                          #'()) (~? r) ...) env)] ; the env returns a list of parts of speech

    ;arrays -> noun nodes, from a sequence of scalar nodes (in the parser bc complete separation lex/parse)
    #;[(_ (a:array/j r ...) env)
     #'(make-line-ast (a.node (~? r) ...) env)]
    ;scalars -> noun nodes
    #;[(_ (s:scalar/j r ...) env)
     #'(make-line-ast (s.node (~? r) ...) env)]

    ;self-evaluating forms
    #;[(_ (cavn:cavn/j r ...+) env)
     #`(make-line-ast (cavn.node r ...) env)]
    ;termination
    [(_ (cavn:cavn/j) env)
     #`cavn.node]
    
    ;monad 1
    [(_ (n:noun/j v1:verb/j v2:verb/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #'(make-line-ast (#,(make-node
                          #'(noun)
                          #'verb-app
                          #'()
                          #`#,(make-node #'(verb) #'v1.symbol #'() #'() #'())
                          #'n.node) v2 (~? next) (~? r) ...) env)]
    ;dyad 2
    [(_ (n1:noun/j v:verb/j n2:noun/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node
                          #'(noun)
                          #'verb-app
                          #'n2.node
                          #`#,(make-node #'(verb) #'v.symbol #'() #'() #'())
                          #'n1.node) (~? next) (~? r) ...) env)]
    ;conjunction 4 (can yield any part of speech, for now only verb)
    [(_ (vn1:vn/j c:conjunction/j vn2:vn/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node
                          #'(verb)
                          #'conjunction-app
                          #'vn2.node
                          #'c.node
                          #'vn1.node) (~? next) (~? r) ...) env)]
    ;fork 5 (can yield any part of speech?)
    [(_ (v1:verb/j v2:verb/j vn:vn/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node
                          #'(verb)
                          #'fork
                          #'vn.node
                          #'v2.node
                          #'v1.node) (~? next) (~? r) ...) env)]
    ;monad 0 brittle implementation using noun elimination for p&e 'anything' since a sequence of noun means an array
    [(_ ((~optional o:cav/j) n:noun/j v:verb/j (~optional (~or* next:copula/j next:lparen)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node
                                 #'(noun)
                                 #'verb-app
                                 #'()
                                 #`#,(make-node #'(verb) #'v.symbol #'() #'() #'())
                                 #'n.node) (~? next) (~? r) ...) env)]
    ;adverb 3 (can yield any part of speech, for now only verb)
    [(_ ((~optional o) a:adverb/j (~or* vn:verb/j vn:noun/j) (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node
                                 #'(verb)
                                 #'adverb-app
                                 #'vn.node
                                 #'a.node
                                 #'()) (~? next) (~? r) ...) env)]
    ;hook/adverb 6
    [(_ ((~optional o) cavn1:cavn/j cavn2:cavn/j (~optional (~or* next:copula/j next:lparen)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node
                                 #'(verb)
                                 #'hook
                                 #'()
                                 #'cavn2.node
                                 #'cavn1.node) (~? next) (~? r) ...) env)]
    ;is 7
    [(_ ((~optional o) v:cavn/j _:copula/j n:name/j r ...) env)
     #`(make-line-ast ((~? o) #,(make-node
                                 #'() ;unit type for assignment
                                 #'assignment
                                 #'()
                                 #'n.node
                                 #'v.node) (~? r) ...) env)]
    ;parentheses 8
    [(_ ((~optional o) rparen cavn:cavn/j lparen r ...) env)
     #`(make-line-ast ((~? o) #,(local-expand #'(make-line-ast cavn #,(hasheq)) 'expression #f) (~? r) ...) env)]))

(define-syntax (compile stx)
  (syntax-parse stx
    [(_ (compiled ...) (ast asts ...) env)
     #`(compile (values compiled ... #,(local-expand #`(compile-node ast #,(hasheq)) 'expression #f)) (asts ...) env)]
    [(_ compiled () env)
     #'compiled]))

; post order traversal
(define-syntax (compile-node stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    ;nouns
    ;arrays (that's a dirty hacky way of differentiating arrays from scalars)
    [(_ ((noun) (n ...+) () () ()) env)
     #`(array/syntax array tr:list unsafe-list->array #[#,@(reverse (syntax->list #'((#%datum . n) ...)))])]
    ;scalars
    [(_ ((noun) n () () ()) env)
     #'(#%datum . n)]
    ;verb application
    #;[(_ ((verb) n () () ()) env) ;THIS IS THE 1ST-CLASS VERB CASE, WHICH REQUIRES MULTIMETHODS
     #`#,(local-expand #'(primitive-env-ref n) 'expression #f)]

    [(_ ((noun) verb-app () v n:noun/j) env) ;preliminary compilation of noun node, fall-through on next 'compile-node'
     #`(compile-node ((noun) verb-app () v #,(local-expand #'(compile-node n.node env) 'expression #f)) env)]
    [(_ ((noun) verb-app () v:verb/j ((~literal #%datum) . d)) env) ;scalar monad case
     #`(#,(local-expand #`(primitive-env-ref v.symbol scalar-arg) 'expression #f)
        (#%datum . d))]
    [(_ ((noun) verb-app () v:verb/j a) env) ;array monad case (brittle matching by elimination for now)
     #`(#,(local-expand #`(primitive-env-ref v.symbol array-arg) 'expression #f)
        a)]


    [(_ ((noun) verb-app n1:noun/j v n2:noun/j) env) ;preliminary compilation of noun node, fall-through on next 'compile-node'
     #`(compile-node ((noun) verb-app
        #,(local-expand #'(compile-node n1.node env) 'expression #f)
        v
        #,(local-expand #'(compile-node n2.node env) 'expression #f)) env)]
    [(_ ((noun) verb-app ((~literal #%datum) . d1) v:verb/j ((~literal #%datum) . d2)) env) ;scalar scalar dyad case
     #`(#,(local-expand #`(primitive-env-ref v.symbol scalar-arg scalar-arg) 'expression #f)
        (#%datum . d1) (#%datum . d2))]
    [(_ ((noun) verb-app a v:verb/j ((~literal #%datum) . d)) env) ;array scalar dyad case (scalar lifted to array)
     #`(#,(local-expand #`(primitive-env-ref v.symbol array-arg array-arg) 'expression #f)
        a (array/syntax array tr:list unsafe-list->array #[#,@(reverse (syntax->list #'((#%datum . d))))]))]
    [(_ ((noun) verb-app ((~literal #%datum) . d) v:verb/j a) env) ;scalar array dyad case (same the other way round)
     #`(#,(local-expand #`(primitive-env-ref v.symbol array-arg array-arg) 'expression #f)
        (array/syntax array tr:list unsafe-list->array #[#,@(reverse (syntax->list #'((#%datum . d))))]) a)]
    [(_ ((noun) verb-app a1 v:verb/j a2) env) ;array array dyad case (array array cases cannot be fused bc we don't know which to lift!)
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