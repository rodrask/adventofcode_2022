#lang racket

(define (aggregate in-port)
  (letrec ([aggregate-until-empty
            (lambda (lines)
              (sequence-fold +
                             0
                             (sequence-map string->number
                                           (stop-before lines (negate non-empty-string?)))))]
           [seq-iter
            (lambda (seq aggr-values)
              (let ([aggr (aggregate-until-empty seq)])
                (if (= 0 aggr) (reverse aggr-values) (seq-iter seq (cons aggr aggr-values)))))])
    (seq-iter (in-lines in-port) '())))

(define (day-1 input top-k)
  (call-with-input-file
   input
   (lambda (in-port) (for/sum ([s (take (sort (aggregate in-port) >=) top-k)]) s))))

(day-1 "day_1.txt" 1)
(day-1 "day_1.txt" 3)
