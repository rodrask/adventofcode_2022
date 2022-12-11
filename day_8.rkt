#lang racket
(require threading)
(require math)

(define (count-visible-trees trees-line)
  (define-values (r _)
    (for/fold ([visible-idxs '()] [max-height -1])
              ([tree trees-line] [idx (in-naturals)] #:when (> tree max-height))
      (values (cons idx visible-idxs) tree)))
  (reverse r))

(define (char->number digit)
  (define zero-value (char->integer #\0))
  (- (char->integer digit) zero-value))

(define (line->trees line)
  (~> line string-trim string->list (map char->number _)))

(define (count-visibles tree-map)
  (define visible-places (mutable-set))
  (define n-rows (vector-ref (array-shape tree-map) 0))
  (define n-cols (vector-ref (array-shape tree-map) 1))
  ; rows
  (for ([row (in-array-axis tree-map 0)]
        [row-idx (in-naturals)]
        #:do [(define reverse-row (~> row in-array sequence->list reverse))
              (define col-idxs (count-visible-trees (in-array row)))
              (define reverse-col-idxs (count-visible-trees reverse-row))])

    (for-each (lambda (i) (set-add! visible-places (cons row-idx i))) col-idxs)
    (for-each (lambda (i) (set-add! visible-places (cons row-idx (- n-cols i 1)))) reverse-col-idxs))
  ; cols
  (for ([col (in-array-axis tree-map 1)]
        [col-idx (in-naturals)]
        #:do [(define reverse-col (~> col in-array sequence->list reverse))
              (define row-idxs (count-visible-trees (in-array col)))
              (define reverse-row-idxs (count-visible-trees reverse-col))])

    (for-each (lambda (i) (set-add! visible-places (cons i col-idx))) row-idxs)
    (for-each (lambda (i) (set-add! visible-places (cons (- n-rows i 1) col-idx))) reverse-row-idxs))
  (set-count visible-places))

(define (count-view-length trees-line height)
  (for/last ([tree (in-array trees-line)] [i (in-naturals 1)])
    #:final (>= tree height)
    i))
(define (tree-lines tree-map row col)
  (let ([row-before (array-slice-ref tree-map (list row (::  (sub1 col) #f -1)))]
        [row-after (array-slice-ref tree-map (list row (:: (add1 col) #f 1)))]

        [col-before (array-slice-ref tree-map (list (:: (sub1 row) #f -1) col))]
        [col-after (array-slice-ref tree-map (list (:: (add1 row) #f 1) col))])
    (list row-before col-before row-after col-after)))

(define (scenic-score tree-map)
  (define n-rows (vector-ref (array-shape tree-map) 0))
  (define n-cols (vector-ref (array-shape tree-map) 1))
  (for*/fold ([max-score 0]) 
             ([row (range 1 (sub1 n-rows))] [col (range 1 (sub1 n-cols))]
             #:do [(define val (array-ref tree-map (vector row col)))
                   (define lines (tree-lines tree-map row col))
                   (define view-legnths (map (lambda (l) (count-view-length l val)) lines))])
      (max max-score (foldl * 1 view-legnths))))

(define (day-8 input proc)
  (call-with-input-file input
                        (lambda~> port->lines (map line->trees _) (list*->array _ number?) proc)))

(day-8 "day_8.txt" count-visibles)
(day-8 "day_8.txt" scenic-score)
