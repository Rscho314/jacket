#lang racket

; Basically, all "type checking" depends on the definitions in this file

(require (prefix-in tr: typed/racket)
         (for-syntax syntax/parse
                     (only-in "lex.rkt" make-node)))
(provide (for-syntax (all-defined-out)))

(begin-for-syntax
  
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
    [pattern copula])
  
  (define-syntax-class noun/j
    #:literal-sets (execution-patterns)
    [pattern (noun n _ _ _) #:attr symbol #'n])
  
  (define-syntax-class adverb/j
    #:literal-sets (execution-patterns)
    [pattern (adverb n _ _ _) #:attr symbol #'n])

  (define-syntax-class conjunction/j
    #:literal-sets (execution-patterns)
    [pattern (conjunction n _ _ _) #:attr symbol #'n])
  
  (define-syntax-class verb/j
    #:literal-sets (execution-patterns)
    [pattern (verb n _ _ _) #:attr symbol #'n])

  (define-syntax-class name/j
    #:literal-sets (execution-patterns)
    [pattern (name n _ _ _) #:attr symbol #'n])

  (define-syntax-class cavn/j
    #:literal-sets (execution-patterns)
    [pattern :conjunction/j]
    [pattern :adverb/j]
    [pattern :verb/j]
    [pattern :noun/j])

(define-syntax-class cav/j
    #:literal-sets (execution-patterns)
    [pattern :conjunction/j]
    [pattern :adverb/j]
    [pattern :verb/j])
  
  (define-syntax-class vn/j
    #:literal-sets (execution-patterns)
    [pattern :verb/j]
    [pattern :noun/j])

  (define-syntax-class lparen
    [pattern 'lparen]))