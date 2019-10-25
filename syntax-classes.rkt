#lang racket

; Basically, all "type checking" depends on the definitions in this file

(require (for-syntax racket/base
                     syntax/parse
                     ;syntax/parse/define
                     ;(only-in "lex.rkt" make-node)
                     ;(only-in (prefix-in tr_ typed/racket) tr_list)
                     ;math/array
                     ))
(provide (for-syntax (except-out (all-defined-out)
                                 node)))

(begin-for-syntax
  
  (define-literal-set execution-patterns
    #:for-syntax #:datum-literals (->
                                   Monadic
                                   Scalar
                                   Array
                                   Number
                                   scalar-arg
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

  #;(define-syntax-class node
    [pattern ((n ms ma ds da) s l r)
             #:attr symbol #'s
             #:attr type-nulladic #'n
             #:attr type-monadic-scalar #'ms
             #:attr type-monadic-array #'ma
             #:attr type-dyadic-scalar #'ds
             #:attr type-dyadic-array #'da])

  (define-syntax-class node
    [pattern (t:type s l r)
             #:attr symbol #'s
             #:attr type-nulladic #'t.n
             #:attr type-monadic-scalar #'t.ms
             #:attr type-monadic-array #'t.ma
             #:attr type-dyadic-scalar #'t.ds
             #:attr type-dyadic-array #'t.da])

  (define-syntax-class type
    [pattern (n ms ma ds da)
             #:attr type-nulladic #'n
             #:attr type-monadic-scalar #'ms
             #:attr type-monadic-array #'ma
             #:attr type-dyadic-scalar #'ds
             #:attr type-dyadic-array #'da])
  
  (define-syntax-class noun/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:attr symbol #'n.symbol
             #:attr type-nulladic #'n.type-nulladic
             #:fail-unless (let ([type (syntax->datum (attribute n.type-nulladic))])
                             (or (equal? '(Array Number) type)
                                 (eq? 'Number type)))
             "expected a node of type 'noun'"])
  
  #;(define-syntax-class adverb/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:fail-unless (eq? 'adverb (syntax-e (attribute n.type)))
             "expected a node of type 'adverb'"])

  #;(define-syntax-class conjunction/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:fail-unless (eq? 'conjunction (syntax-e (attribute n.type)))
             "expected a node of type 'conjunction'"])
  
  (define-syntax-class verb/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:attr symbol #'n.symbol
             #:attr type-monadic-scalar #'n.type-monadic-scalar
             #:attr type-monadic-array #'n.type-monadic-array
             #:attr type-dyadic-scalar #'n.type-dyadic-scalar
             #:attr type-dyadic-array #'n.type-dyadic-array
             #:fail-unless (or
                            (equal? '(-> Number Number) (syntax->datum (attribute n.type-monadic-scalar)))
                            (equal? '(-> (Array Number) (Array Number)) (syntax->datum (attribute n.type-monadic-array)))
                            (equal? '(-> Number Number Number) (syntax->datum (attribute n.type-dyadic-scalar)))
                            (equal? '(-> (Array Number) (Array Number) (Array Number)) (syntax->datum (attribute n.type-dyadic-array))))
             "expected a node of type 'verb'"])

  #;(define-syntax-class name/j
    #:literal-sets (execution-patterns)
    [pattern n:node
             #:fail-unless (eq? 'name (syntax-e (attribute n.type)))
             "expected a node of type 'name'"])

  (define-syntax-class cavn/j
    #:literal-sets (execution-patterns)
    #;[pattern conjunction/j]
    #;[pattern adverb/j]
    [pattern verb/j]
    [pattern noun/j])

(define-syntax-class cav/j
    #:literal-sets (execution-patterns)
    #;[pattern conjunction/j]
    #;[pattern adverb/j]
    [pattern verb/j])
  
#;(define-syntax-class vn/j
  #:literal-sets (execution-patterns)
  [pattern verb/j]
  [pattern noun/j]))