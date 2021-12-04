#lang racket

(define (parse-line line)
  (define split (string-split line " "))
  (cond [(empty? split)
         (datum->syntax #f "")]
        [else (define command (car split))
              (define arg (second split))
              (datum->syntax #f `(,(string->symbol command) ,(string->number arg)))]))

(define (read-syntax path port)
  (define src-datums (map parse-line (port->lines port)))
  (datum->syntax #f `(module day2 racket
                       (define depth 0)
                       (define pos 0)
                       (define (forward x)
                         (set! pos (+ pos x)))
                       (define (down x)
                         (set! depth (+ depth x)))
                       (define (up x)
                         (set! depth (- depth x)))
                       ,@src-datums
                       (* depth pos))))

(provide read-syntax)
