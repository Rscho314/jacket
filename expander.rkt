#lang racket

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse)
         math/array
         "syntax-classes.rkt")

(provide (rename-out [module-begin/j #%module-begin]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (())) #'(tr:#%module-begin)] ; empty program
    [(_ prg) #`(tr:#%module-begin
                (make-ast () prg #,(hasheq)))])) ;top-level env

(define-syntax (make-ast stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    [(_ (line-trees ...) (lines ... line) env)
     #`(make-ast (#,(local-expand #`(make-line-ast line #,(hasheq 'a '(adverb verb noun))) 'expression #f) line-trees ... ) (lines ...) env)] ; line-level env
    [(_ trees () env)
     #''trees])) ; termination condition yields a tree per line

(define-syntax (make-line-ast stx)
  (syntax-parse stx
    #:literal-sets (execution-patterns)
    #|The type of some names is a union-type, so on simple type inference we get a
      list representing the type from the env, which is then matched against the
      syntax-classes to drive parsing (e.g. :verb/j matches a list containing 'verb, etc.).
      We'll use that to generate parse trees corresponding to all possible cases|#
    ;reference to an identifier
    #;[(_ (n:name/j tokens ...) env)
     #`(make-line-ast ((#,(hash-ref (syntax-e #'env) (syntax-e #'n.symbol)) (name n.symbol)) tokens ...) env)] ; the env returns a list of parts of speech
    ;monad 1
    [(_ (n:noun/j v1:verb/j v2:verb/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #'(make-line-ast (((noun) (monad-app v1.symbol n)) v2 (~? next) (~? r) ...) env)]
    ;dyad 2
    [(_ (n1:noun/j v:verb/j n2:noun/j (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #'(make-line-ast (((noun) (dyad-app v.symbol n2 n1)) (~? next) (~? r) ...) env)]
    ;conjunction 4
    ;monad 0 (we are sure that this yields a noun ; after dyad 2 bc of term ... for p&e 'anything'
    [(_ ((~optional o) n:noun/j v:verb/j (~optional (~or* next:copula/j next:lparen)) r ...) env)
     #`(make-line-ast ((~? o) ((noun) (monad-app v.symbol n)) (~? next) (~? r) ...) env)]
    ;adverb 3 (can yield any part of speech, for now only verb)
    [(_ ((~optional o) a:adverb/j (~or* vn:verb/j vn:noun/j) (~optional (~or* next:copula/j next:lparen next:adverb/j next:verb/j next:noun/j)) r ...) env)
     #'(make-line-ast ((~? o) ((verb) (adverb-app a.symbol vn)) (~? next) (~? r) ...) env)]
    ;is 7
    [(_ ((~optional o) v:cavn/j _:copula/j n:name/j r ...) env)
     #'(make-line-ast ((~? o) (assignment n.symbol (v.pos v.symbol)) (~? r) ...) env)]
    ;termination
    [(_ (tree) env)
     #'tree]))
