#lang racket

(require "parser.rkt")
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer
       [(char-set "\n1234567890, ") lexeme]
       [any-char (next-token)]))
    (bf-lexer port))
  next-token)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module day4 racket
                          ,parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)
