#lang racket
(require threading)
(require rebellion/base/range)

(define (full-contain? ranges)
  (let ([range-x (first ranges)]
        [range-y (second ranges)])
  (or (range-encloses? range-x range-y)
      (range-encloses? range-y range-x))))

(define (parse-job-range range-str)
  (~>> range-str (string-split _ "-") (map string->number) (apply closed-range)))

(define (parse-jobs-pair pair-str)
  (~> pair-str string-trim (string-split _ "," ) (map parse-job-range _)))

(define (parse-part-1 in-sequence)
    (~>> in-sequence (sequence-map parse-jobs-pair) (sequence-count full-contain?)))

(define (parse-part-2 in-sequence)
    (~>> in-sequence (sequence-map parse-jobs-pair) (sequence-count (lambda (rs) (apply range-overlaps? rs)))))

(define (day-4 input parse-proc)
  (call-with-input-file
   input
   (lambda~> in-lines parse-proc)))

(day-4 "day_4.txt" parse-part-1)
(day-4 "day_4.txt" parse-part-2)
