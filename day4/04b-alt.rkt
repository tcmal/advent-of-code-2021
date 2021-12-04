#lang racket

(require racket/file)

(define (parse-row x)
  (map string->number (filter (lambda (x) (< 0 (string-length x))) (regexp-split #rx" +" x))))

(define (parse-board x)
  (map parse-row (string-split x "\n")))

(define (get-numbers x)
  (define number-line (car (string-split x "\n\n")))
  (map string->number (string-split number-line ",")))

(define (get-boards x)
  (define boards (cdr (string-split x "\n\n")))
  (map parse-board boards))

(define (subsetOf xs ys)
  (subset? (list->set xs) (list->set ys)))

(define (transpose xs)
  (apply map list xs))

(define (check-win board numbers)
  (define winningColumns (filter (lambda (x) (subsetOf x numbers)) board))
  (define winningRows (filter (lambda (x) (subsetOf x numbers)) (transpose board)))
  (cond [(not (null? winningColumns)) (car winningColumns)]
        [(not (null? winningRows)) (car winningRows)]
        [else #f]))

(define input (file->string "./input"))
(define numbers (get-numbers input))
(define boards (get-boards input))

(define ht (make-hash))
(for* ([rnd (length numbers)]
       [board boards]
       #:unless (hash-ref ht board #f)
       )
  (cond [(check-win board (take numbers rnd)) (hash-set! ht board rnd)]))

(define maxTurn 0)
(define lastBoard #f)
(hash-for-each ht (lambda (k v) (cond [(> v maxTurn) (set! maxTurn v) (set! lastBoard k)])))

(*
 (foldr + 0 (filter (lambda (x) (not (member x (take numbers maxTurn)))) (flatten lastBoard)))
 (list-ref numbers (- maxTurn 1)))
