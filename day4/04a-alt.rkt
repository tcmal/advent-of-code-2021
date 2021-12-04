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

(define winningBoard #f)
(define winningRun #f)
(define wonOnRound 0)
(for* ([rnd (length numbers)]
       [board boards]
       #:unless winningRun
       )
  (set! winningRun (check-win board (take numbers rnd)))
  (set! winningBoard board)
  (set! wonOnRound rnd))

(*
 (foldr + 0 (filter (lambda (x) (not (member x (take numbers wonOnRound)))) (flatten winningBoard)))
 (list-ref numbers (- wonOnRound 1)))
