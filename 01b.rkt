#lang racket
(require threading)

(define input (file->lines "inputs/01"))

(define (map-differing-lengths f . arrs)
  (define minLength (apply min (map length arrs)))
  (define newLists (map (lambda (x) (take x minLength)) arrs))
  (apply map (cons f newLists)))

(define (build-windows xs)
  (map-differing-lengths list xs (cdr xs) (drop xs 2)))

(define (diff-pairs xs)
  (map-differing-lengths - (cdr xs) xs))

(~>> input
    (map string->number)
    (build-windows)
    (map (lambda (x) (apply + x)))
    (diff-pairs)
    (filter positive?)
    (length _))
