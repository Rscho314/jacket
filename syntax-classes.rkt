#lang racket

; Basically, all "type checking" depends on the definitions in this file

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse))
(provide (for-syntax (all-defined-out)))

(begin-for-syntax

(define (make-node type name left middle right)
  (list type name left middle right))
  
  (define-literal-set execution-patterns
    #:for-syntax #:datum-literals (scalar-arg
                                   array-arg
                                   assignment
                                   reference
                                   name
                                   verb
                                   verb-app
                                   noun
                                   adverb
                                   adverb-app
                                   conjunction
                                   conjunction-app
                                   fork
                                   hook
                                   copula
                                   paren) ())
  
  (define-syntax-class copula/j
    #:literal-sets (execution-patterns)
    [pattern ((copula) _)])
  
  (define-splicing-syntax-class array/j
    #:literal-sets (execution-patterns)
    [pattern (~seq s0:scalar/j s1:scalar/j ...+)
             #:attr node #`#,(make-node #'(noun) #`(s0.symbol s1.symbol ...) #'() #'() #'())])
  
  (define-syntax-class scalar/j
    #:literal-sets (execution-patterns)
    [pattern (~and ((noun) n))
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(noun) #'n #'() #'() #'())])
  
  (define-splicing-syntax-class noun/j
    #:literal-sets (execution-patterns)
    [pattern n:array/j #:attr node #'n.node]
    [pattern n:scalar/j #:attr node #'n.node]
    [pattern ((noun) n l m r)
             #:attr symbol #'n
             #:attr node #'((noun) n l m r)])
  
  (define-syntax-class adverb/j
    #:literal-sets (execution-patterns)
    [pattern ((adverb) n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(adverb) #'n #'() #'() #'())]
    [pattern ((adverb) n l m r)
             #:attr symbol #'n
             #:attr node #'((adverb) n l m r)])

  (define-syntax-class conjunction/j
    #:literal-sets (execution-patterns)
    [pattern ((conjunction) n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(conjunction) #'n #'() #'() #'())]
    [pattern ((conjunction) n l m r)
             #:attr symbol #'n
             #:attr node #'((conjunction) n l m r)])
  
  (define-syntax-class verb/j
    #:literal-sets (execution-patterns)
    [pattern ((verb) n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(verb) #'n #'() #'() #'())]
    [pattern ((verb) n l m r)
             #:attr symbol #'n
             #:attr node #'((verb) n l m r)])

  (define-syntax-class name/j
    #:literal-sets (execution-patterns)
    [pattern (name n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(name) #'n #'() #'() #'())]
    [pattern ((name) n l m r)
             #:attr symbol #'n
             #:attr node #'((name) n l m r)])

  (define-splicing-syntax-class cavn/j
    #:literal-sets (execution-patterns)
    [pattern n:conjunction/j
             #:attr node #'n.node]
    [pattern n:adverb/j
             #:attr node #'n.node]
    [pattern n:verb/j
             #:attr node #'n.node]
    [pattern n:noun/j
             #:attr node #'n.node])

(define-syntax-class cav/j
    #:literal-sets (execution-patterns)
    [pattern n:conjunction/j
             #:attr node #'n.node]
    [pattern n:adverb/j
             #:attr node #'n.node]
    [pattern n:verb/j
             #:attr node #'n.node])
  
  (define-splicing-syntax-class vn/j
    #:literal-sets (execution-patterns)
    [pattern n:verb/j
             #:attr node #'n.node]
    [pattern n:noun/j
             #:attr node #'n.node])

  (define-syntax-class lparen
    [pattern 'lparen]))