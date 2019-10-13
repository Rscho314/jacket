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
    ;self-evaluating forms
    [(_ (cavn:cavn/j) env)
     #`cavn.node]
    ;monad 1
    [(_ (n:noun/j v1:verb/j v2:verb/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #'(make-line-ast (#,(make-node
                          #'(noun)
                          #'monad-app
                          #'()
                          #'v1.node
                          #'n.node) v2 (~? next) (~? r) ...) env)]
    ;dyad 2
    [(_ (n1:noun/j v:verb/j n2:noun/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node
                          #'(noun)
                          #'dyad-app
                          #'n2.node
                          #'v.node
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
    ;monad 0 (we are sure that this yields a noun ; after dyad 2 bc of term ... for p&e 'anything'
    [(_ ((~optional o) n:noun/j v:verb/j (~optional (~or* next:copula/j next:lparen)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node
                                 #'(noun)
                                 #'monad-app
                                 #'()
                                 #'v.node
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
     #`(make-line-ast ((~? o) #,(local-expand #'(make-line-ast cavn #,(hasheq)) 'expression #f) (~? r) ...) env)]
    ;termination
    [(_ (tree) env)
     #'tree]))

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
    ;literals
    [(_ ((noun) n () () ()) env)
     #'(#%datum . n)]
    [(_ ((verb) n () () ()) env)
     #`#,(local-expand #'(primitive-env-ref n) 'expression #f)]
    [(_ ((noun) monad-app () v n) env)
     #`(#,(local-expand #'(compile-node v env) 'expression #f) #,(local-expand #'(compile-node n env) 'expression #f))]))

(define-syntax (primitive-env-ref stx)
  (syntax-parse stx
    ;verbs
    [(_ ^)
     #'(case-lambda ;optimize to not use case-lambdas
         [(y) (exp y)]
         [(x y) (expt x y)])]))