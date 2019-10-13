#lang racket
; The lexer unfortunately cannot be discarded, as quote has
; syntactic meaning in scheme.
; TODO: handle parentheses better
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex/j)

(define-lex-abbrevs
  [name (:: (:+ alphabetic)
            (:* (:or alphabetic numeric))
            (:* (:: (:? #\_) (:+ (:or alphabetic numeric)))))])

; minimal lexer that rejects nothing (i.e. incomplete language definition)
; for now, only character sequences with semantic meaning need a case definition (e.g. =:)
(define lexer/j
  (lexer
   ;adverbs
   [#\/ '((adverb) /)]
   ;assignment
   ["=:" '((copula) =:)]
   ;conjunctions
   ["@:" '((conjunction) @:)]
   ;names
   [name (list 'name (string->symbol lexeme))]
   ;nouns
   ; re-use racket numbers
   [(:+ numeric) (list '(noun) (read (open-input-string lexeme)))]
   ;parentheses
   [#\( 'lparen]
   [#\) 'rparen]
   ;verbs
   [#\^ '((verb) ^)]
   
   [#\space (lexer/j input-port)]
   [(eof) 'eof]
   [#\newline #\newline]))

; yields a program list, with one nested list per line, all reversed
;removes empty lines
(define (lex/j ip)
  (define (run acc-file acc-line)
    (let ([tok (lexer/j ip)]
          [cons-line (cons acc-line acc-file)])
      (cond [(and (equal? tok 'eof) (not (equal? acc-line empty))) cons-line]
            [(and (equal? tok #\newline) (not (equal? acc-line empty))) (run cons-line '())]
            [(and (equal? tok 'eof) (equal? acc-line empty)) acc-file]
            [(and (equal? tok #\newline) (equal? acc-line empty)) (run acc-file '())]
            [else (run acc-file (cons tok acc-line))])))
(run '() '()))

#;(lex/j (open-input-string "r=:3"))