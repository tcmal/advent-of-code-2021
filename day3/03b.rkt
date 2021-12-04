#lang racket

(define (mode xs)
  (define ht (make-hash))
  (define max-key '0)
  (for ([x xs])
    (define new-val (+ 1 (hash-ref ht x 0)))
    (hash-set! ht x new-val)
    (if (> new-val (hash-ref ht max-key 0)) (set! max-key x) 3))
  (cond [(= (hash-ref ht #\0 0) (hash-ref ht #\1 0)) #f]
        [else max-key]))

(define (flip x)
  (cond [(char=? x #\0) #\1]
                    [else #\0]))

(define (bit-criteria lines least [fallback (cond [least #\0] [else #\1])] [n 0])
  (cond [(= (length lines) 1) (car lines)]
        [else
         (define bits (map (curryr string-ref n) lines))
         (define bMode (mode bits))
         (define criteria (cond [(not bMode) fallback]
                                [least (flip bMode)]
                                [else bMode]))
         (bit-criteria (filter (lambda (x) (char=? criteria (string-ref x n)))
                               lines)
                       least fallback (+ n 1))]))

(define input (open-input-file "input"))
(define lines (port->lines input))

(define oxy (string->number (bit-criteria lines #f) 2))
(define co2 (string->number (bit-criteria lines #t) 2))

(* oxy co2)
