#lang racket

; Basically, all "type checking" depends on the definitions in this file

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse)
         math/array)
(provide (for-syntax (all-defined-out)))

(begin-for-syntax

(define (make-node type name left middle right)
  (list type name left middle right))
  
  (define-literal-set execution-patterns
    #:for-syntax #:datum-literals (assignment
                                   reference
                                   name
                                   verb
                                   noun
                                   monad-app
                                   dyad-app
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
  
  #;(define-splicing-syntax-class noun/j
    #:literal-sets (execution-patterns)
    ; for simplicity, everything is lifted to an array for now
    ; gotcha: data in array constructor syntax
    ; is normally implicitely quoted, but here we need explicit quoting!
    ; todo: this uses racket number syntax, change to J (requires lexer change)
    [pattern (_ ... (~literal noun) _ ...)]
    [pattern (~seq ((_ ... noun _ ...) n) ...+)
             #:attr node #'(noun (Array tr:Number) (array #(#,@(reverse (syntax->list #'((quote n) ...))))))])

  (define-syntax-class noun/j
    #:literal-sets (execution-patterns)
    [pattern ((noun) n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(noun) #'n #'() #'() #'())])
  
  (define-syntax-class adverb/j
    #:literal-sets (execution-patterns)
    [pattern ((adverb) n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(adverb) #'n #'() #'() #'())])

  (define-syntax-class conjunction/j
    #:literal-sets (execution-patterns)
    [pattern ((conjunction) n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(conjunction) #'n #'() #'() #'())])
  
  (define-syntax-class verb/j
    #:literal-sets (execution-patterns)
    [pattern ((verb) n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(verb) #'n #'() #'() #'())])

  (define-syntax-class name/j
    #:literal-sets (execution-patterns)
    [pattern (name n)
             #:attr symbol #'n
             #:attr node #`#,(make-node #'(name) #'n #'() #'() #'())])

  (define-syntax-class cavn/j
    #:literal-sets (execution-patterns)
    [pattern n:conjunction/j
             #:attr node #`#,(make-node #'(conjunction) #'n.symbol #'() #'() #'())]
    [pattern n:adverb/j
             #:attr node #`#,(make-node #'(adverb) #'n.symbol #'() #'() #'())]
    [pattern n:verb/j
             #:attr node #`#,(make-node #'(verb) #'n.symbol #'() #'() #'())]
    [pattern n:noun/j
             #:attr node #`#,(make-node #'(noun) #'n.symbol #'() #'() #'())])

  (define-syntax-class vn/j
    #:literal-sets (execution-patterns)
    [pattern n:verb/j
             #:attr node #`#,(make-node #'(verb) #'n.symbol #'() #'() #'())]
    [pattern n:noun/j
             #:attr node #`#,(make-node #'(noun) #'n.symbol #'() #'() #'())])

  (define-syntax-class lparen
    [pattern 'lparen]))