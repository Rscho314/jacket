#lang racket

(require (for-syntax syntax/parse
                     (only-in "lex.rkt"
                              make-node
                              make-type))
         "syntax-classes.rkt")
(provide make-ast)

;only handles simple types, not 'case->' types!
(define-syntax (infer-type stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    [(_ a (-> b c) _)
     #:when (equal? (syntax-e #'a) (syntax-e #'b))
     #`#,(make-type #'c)]
    [(_ (Array a) _ (-> (Array b) (Array c)))
     #:when (equal? (syntax-e #'a) (syntax-e #'b))
     #`#,(make-type #'(Array c))]))

(define-syntax (infer-argument stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    [(_ Number)
     #'Scalar]
    [(_ (Array _))
     #'Array]))

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
    ;test cases
    [(_ (n:noun/j) env)
     #'n]
    #;[(_ (v:verb/j) env)
     #''verb]
    ;termination
    #;[(_ (cavn:cavn/j) env)
     #'cavn]
    ;monad 1
    #;[(_ (n:noun/j v1:verb/j v2:verb/j (~optional ((~var next) copula lparen adverb/j verb/j noun/j)) r ...) env)
     #'(make-line-ast (#,(make-node #'noun #'verb-app #f #'v1 #'n) v2 (~? next) (~? r) ...) env)]
    ;dyad 2
    #;[(_ (n1:noun/j v:verb/j n2:noun/j (~optional ((~var next) copula lparen adverb/j verb/j noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node #'noun #'verb-app #'n2 #'v #'n1) (~? next) (~? r) ...) env)]
    ;conjunction 4 (can yield any part of speech, for now only verb)
    #;[(_ (vn1:vn/j c:conjunction/j vn2:vn/j (~optional ((~var next) copula lparen adverb/j verb/j noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node #'verb #'conjunction-app #'vn2 #'c #'vn1) (~? next) (~? r) ...) env)]
    ;fork 5 (can yield any part of speech?)
    #;[(_ (v1:verb/j v2:verb/j vn:vn/j (~optional ((~var next) copula lparen adverb/j verb/j noun/j)) r ...) env)
     #`(make-line-ast (#,(make-node #'verb #'fork #'vn #'v2 #'v1) (~? next) (~? r) ...) env)]
    ;monad 0 brittle implementation using noun elimination for p&e 'anything' since a sequence of noun means an array
    [(_ ((~optional o:cav/j) n:noun/j v:verb/j (~optional ((~var next) (~or* copula lparen))) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node
                                 #`#,(local-expand #'(infer-type n.type-nulladic v.type-monadic-scalar v.type-monadic-array) 'expression #f)
                                 #`(v.symbol Monadic #,(local-expand #'(infer-argument n.type-nulladic) 'expression #f))
                                 #f
                                 #'n) (~? next) (~? r) ...) env)]
    ;adverb 3 (can yield any part of speech, for now only verb)
    #;[(_ ((~optional o) a:adverb/j (~or* vn:verb/j vn:noun/j) (~optional ((~var next) copula lparen adverb/j verb/j noun/j)) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node #'verb #'adverb-app #'vn #'a #f) (~? next) (~? r) ...) env)]
    ;hook/adverb 6
    #;[(_ ((~optional o) cavn1:cavn/j cavn2:cavn/j (~optional ((~var next) (~or* copula lparen))) r ...) env)
     #`(make-line-ast ((~? o) #,(make-node #'verb #'hook #f #'cavn2 #'cavn1) (~? next) (~? r) ...) env)]
    ;reference to an identifier
    #;[(_ (n:name/j r ...) env)
     #`(make-line-ast (#,(make-node (hash-ref (syntax-e #'env) (syntax-e #'n.symbol)) #'reference #f #'n #f) (~? r) ...) env)]
    ;is 7
    #;[(_ ((~optional o) v:cavn/j copula n:name/j r ...) env)
     #`(make-line-ast ((~? o) #,(make-node #f #|unit type for assignment|# #'assignment #f #'n #'v) (~? r) ...) env)]
    ;parentheses 8
    #;[(_ ((~optional o) rparen cavn:cavn/j lparen r ...) env)
     #`(make-line-ast ((~? o) #,(local-expand #'(make-line-ast cavn #,(hasheq)) 'expression #f) (~? r) ...) env)]))
