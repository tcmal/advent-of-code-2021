#lang racket
(require threading)

(define input (file->lines "input"))

(define (map-differing-lengths f . arrs)
  (define minLength (apply min (map length arrs)))
  (define newLists (map (lambda (x) (take x minLength)) arrs))
  (apply map (cons f newLists)))

(define (diff-pairs xs)
  (map-differing-lengths - (cdr xs) xs))

(~>> input
    (map string->number)
    (diff-pairs)
    (filter positive?)
    (length _))
