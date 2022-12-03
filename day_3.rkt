#lang racket
(require threading)

(define (char-score item-char)
  (let ([A (char->integer #\A)]
        [a (char->integer #\a)])
   (if (char-upper-case? item-char)
  (~> item-char char->integer (- _ A) (+ 27))
  (~> item-char char->integer (- _ a) (+ 1)))))

(define (rucksack-score rucksack)
  (let*-values ([(rucksack-half-size) (~> rucksack length (quotient _ 2))]
                [(first-half second-half) (split-at rucksack rucksack-half-size)]
                [(common-char) (set-first (set-intersect (list->set first-half) (list->set second-half)))])
    (char-score common-char)))
(define (line-2-rucksack line)
  (~> line string-trim string->list))

(define (parse-part-1 in-sequence)
    (~> in-sequence (sequence-map line-2-rucksack _) (sequence-map rucksack-score _) (sequence-fold + 0 _)))

(define (parse-part-2 in-sequence)
  (let ([grouped-rucksacks (in-slice 3 in-sequence)]
        [aggregated-score (lambda~> (map line-2-rucksack _) (map list->set _) (apply set-intersect _) set-first char-score)])
  (~> grouped-rucksacks (sequence-map aggregated-score _) (sequence-fold + 0 _))))

(define (day-3 input parse-proc)
  (call-with-input-file
   input
   (lambda~> in-lines parse-proc)))

(day-3 "day_3.txt" parse-part-1)
(day-3 "day_3.txt" parse-part-2)