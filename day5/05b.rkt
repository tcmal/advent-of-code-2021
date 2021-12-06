#lang racket

(require "./05a_grammar.rkt")

(define ventCounts (make-hash))

(define (digit x)
  x)

(define (number . DIGITS)
  (string->number (foldr string-append "" DIGITS)))

(define (coord a _a b)
  (list a b))

(define (line c1 _b _c _d _e c2)
  (define steps (max (abs (- (car c2) (car c1)))
                     (abs (- (second c2) (second c1)))))
  (define mx (/ (- (car c2) (car c1)) steps))
  (define my (/ (- (second c2) (second c1)) steps))
  (for ([s (+ 1 steps)])
    (define v (list (+ (car c1) (* mx s)) (+ (second c1) (* my s))))
    (hash-set! ventCounts v (+ 1 (hash-ref ventCounts v 0)))))

(define (vents l _a . ls)
  (cond [(null? ls) l]
        [else (cons l (apply vents ls))]))

(define (print-board ex ey)
  (for ([y ey])
    (for ([x ex])
      (display (hash-ref ventCounts (list x y) ".")))
    (display "\n")))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define parsed (parse-to-datum (file->string "./input")))
(void (eval parsed ns))
(length (filter (curry <= 2) (hash-values ventCounts)))
