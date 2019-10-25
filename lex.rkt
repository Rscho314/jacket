#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex/j
         make-node
         make-type)

(define (make-node type symbol [left-child #f] [right-child #f])
  (list type symbol left-child right-child))

(define (make-type [nulladic #f] [monadic-scalar #f] [monadic-array #f] [dyadic-scalar #f] [dyadic-array #f])
  (list nulladic monadic-scalar monadic-array dyadic-scalar dyadic-array))

(define-lex-abbrevs
  [name (:: (:+ alphabetic)
            (:* (:or alphabetic numeric))
            (:* (:: (:? #\_) (:+ (:or alphabetic numeric)))))])

(define lexer/j
  (lexer
   ;adverbs
   #;[#\/ (make-node 'adverb (string->symbol lexeme) #f #f #f)]
   ;assignment
   ["=:" 'copula]
   ;conjunctions
   #;["@:" (make-node 'conjunction (string->symbol lexeme) #f #f #f)]
   ;names
   #;[name (make-node 'name (string->symbol lexeme) #f #f #f)]
   ;nouns
   [(:+ numeric) (make-node (make-type 'Number) lexeme)]
   [(:: (:+ numeric) (:+ (:: (:+ #\space) (:+ numeric))))
    (make-node (make-type '(Array Number)) (remove* '(#\space) (string->list lexeme)))]
   ;parentheses
   [#\( 'lparen]
   [#\) 'rparen]
   ;verbs
   [#\^ (make-node '(#f
                     (-> Number Number)
                     (-> (Array Number) (Array Number))
                     (-> Number Number Number)
                     (-> (Array Number) (Array Number) (Array Number))) (string->symbol lexeme))]
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

#;(lex/j (open-input-string "^ 3"))