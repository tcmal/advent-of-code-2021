#lang racket
(define (mode xs)
  (define ht (make-hash))
  (define max-key '0)
  (for ([x xs])
    (define new-val (+ 1 (hash-ref ht x 0)))
    (hash-set! ht x new-val)
    (if (> new-val (hash-ref ht max-key 0)) (set! max-key x) 3))
  max-key)

(define (transpose xss)
  (apply map list xss))

(define (flip x)
  (cond [(string=? x "0") "1"]
        [else "0"]))

(define input (open-input-file "inputs/03"))
(define bit-positions
  (transpose (map (lambda (xs)
                    (map (curry make-string 1) (string->list xs)))
                  (port->lines input))))

(define gamma-str (foldr string-append "" (map mode bit-positions)))
(define epsilon-str (foldr string-append "" (map (compose flip mode) bit-positions)))

(define gamma (string->number gamma-str 2))
(define epsilon (string->number epsilon-str 2))

(* gamma epsilon)
