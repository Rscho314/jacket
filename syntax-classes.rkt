#lang racket

; Basically, all "type checking" depends on the definitions in this file

(require (for-syntax syntax/parse
                     (only-in "lex.rkt" make-node)
                     (only-in (prefix-in tr_ typed/racket) tr_list)
                     math/array))
(provide (for-syntax (except-out (all-defined-out)
                                 node)))

(begin-for-syntax
  
  (define-literal-set execution-patterns
    #:for-syntax #:datum-literals (scalar-arg
                                   array-arg
                                   name
                                   verb
                                   verb-app
                                   noun
                                   adverb
                                   adverb-app
                                   conjunction
                                   conjunction-app
                                   fork
                                   hook) ())

  (define-syntax-class node
    [pattern (t s l m r)
             #:attr type #'t
             #:attr symbol #'s])
  
  (define-syntax-class noun/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:fail-unless (eq? 'noun (syntax-e (attribute n.type)))
             "expected a node of type 'noun'"])
  
  (define-syntax-class adverb/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:fail-unless (eq? 'adverb (syntax-e (attribute n.type)))
             "expected a node of type 'adverb'"])

  (define-syntax-class conjunction/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:fail-unless (eq? 'conjunction (syntax-e (attribute n.type)))
             "expected a node of type 'conjunction'"])
  
  (define-syntax-class verb/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:attr symbol #'n.symbol
             #:fail-unless (eq? 'verb (syntax-e (attribute n.type)))
             "expected a node of type 'verb'"])

  (define-syntax-class name/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:fail-unless (eq? 'name (syntax-e (attribute n.type)))
             "expected a node of type 'name'"])

  (define-syntax-class cavn/j
    #:literal-sets (execution-patterns)
    [pattern conjunction/j]
    [pattern adverb/j]
    [pattern verb/j]
    [pattern noun/j])

(define-syntax-class cav/j
    #:literal-sets (execution-patterns)
    [pattern conjunction/j]
    [pattern adverb/j]
    [pattern verb/j])
  
  (define-syntax-class vn/j
    #:literal-sets (execution-patterns)
    [pattern verb/j]
    [pattern noun/j]))