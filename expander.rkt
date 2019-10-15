#lang racket

(require (only-in (prefix-in tr: typed/racket)
                  tr:#%module-begin)
         "parser.rkt"
         "codegen.rkt"
         (for-syntax syntax/parse))

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




