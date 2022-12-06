#lang racket
(require threading)

(define (slice l offset n)
  (take (drop l offset) n))

(define (find-position line buff-size)
  (define line-size (length line))
  (for/last ([idx-from (range 0 (- line-size buff-size))]
        #:do [(define current-buffer (slice line idx-from buff-size))]
        #:final (boolean? (check-duplicates current-buffer)))
  (+ idx-from buff-size)))

(define (day-6 input [buff-size 4])
  (call-with-input-file
   input
   (lambda~> port->string string->list (find-position _ buff-size))))

(day-6 "day_6.txt")
(day-6 "day_6.txt" 14)