#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex/j
         make-node)

(define (make-node type name left middle right)
  (list type name left middle right))

(define-lex-abbrevs
  [name (:: (:+ alphabetic)
            (:* (:or alphabetic numeric))
            (:* (:: (:? #\_) (:+ (:or alphabetic numeric)))))])

(define lexer/j
  (lexer
   ;adverbs
   [#\/ (make-node 'adverb (string->symbol lexeme) #f #f #f)]
   ;assignment
   ["=:" 'copula]
   ;conjunctions
   ["@:" (make-node 'conjunction (string->symbol lexeme) #f #f #f)]
   ;names
   [name (make-node 'name (string->symbol lexeme) #f #f #f)]
   ;nouns
   [(:+ numeric) (make-node 'noun lexeme #f #f #f)]
   [(:: (:+ numeric) (:+ (:: (:+ #\space) (:+ numeric))))
    (make-node 'noun (remove* '(#\space) (string->list lexeme))
               #f #f #f)]
   ;parentheses
   [#\( 'lparen]
   [#\) 'rparen]
   ;verbs
   [#\^ (make-node 'verb (string->symbol lexeme) #f #f #f)]
   ;other punctuation
   [#\space (lexer/j input-port)]
   [(eof) 'eof]
   [#\newline 'eol]))

(define (lex/j ip)
  (define (run acc-file acc-line)
    (let ([tok (lexer/j ip)]
          [cons-line (cons acc-line acc-file)])
      (cond [(and (equal? tok 'eof) (not (equal? acc-line empty))) cons-line]
            [(and (equal? tok 'eol) (not (equal? acc-line empty))) (run cons-line '())]
            [(and (equal? tok 'eof) (equal? acc-line empty)) acc-file]
            [(and (equal? tok 'eol) (equal? acc-line empty)) (run acc-file '())]
            [else (run acc-file (cons tok acc-line))])))
(run '() '()))