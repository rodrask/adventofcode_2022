#lang racket
(require threading)

(struct crates-move (size from to))
(define (push! stacks pos item)
  (vector-set! stacks pos (append item (vector-ref stacks pos))))

(define (pull! stacks pos [size 1])
  (let*-values ([(stack-for-pull) (vector-ref stacks pos)]
         [(return-value rest-value) (split-at stack-for-pull size)])
  (vector-set! stacks pos rest-value)
  return-value))

(define (elementary-move! stacks from to)
  (push! stacks to (pull! stacks from)))

(define (move-v1! stacks move)
  (for ([_ (in-range (crates-move-size move))])
    (elementary-move! stacks (crates-move-from move) (crates-move-to move))))

(define (move-v2! stacks move)
  (define moved-items (pull! stacks (crates-move-from move) (crates-move-size move)))
  (push! stacks (crates-move-to move) moved-items))

(define (show-crates stacks)
  (println "State:")
  (for ([s stacks]
        [idx (in-naturals 1)])
      (printf "~s : ~s ~n" idx (reverse s))))

(define (crates-top crates)
  (list->string (for/list ([c crates]
             #:when (not (empty? c)))
    (~> c first (string-ref _ 1)))))

(define (fill-row! stacks row-items)
  (for ([item row-items]
        [idx (in-naturals)]
        #:do [(define trimed-item (~> item list->string string-trim))]
        #:when (non-empty-string? trimed-item))
      (push! stacks idx (list trimed-item)))
  stacks)

(define (parse-crates-stack lines-seq)
  (let* ([state (~> lines-seq (stop-before _ (negate non-empty-string?)) sequence->list reverse)]
         [n-stacks (~> state first (in-slice 4 _ ) sequence-length)]
         [result (make-vector n-stacks '())])
  (for ([line (rest state)])
    (fill-row! result (in-slice 4 line)))
  result))
  
(define (parse-move-line line)
  (match (~> line string-trim (string-split _ " ")) 
    [(list "move" size "from" from "to" to)  
      (crates-move (string->number size) (~> from string->number sub1) (~> to string->number sub1))]
    [_ #f]))

(define (parse-line lines-seq move-proc!)
  (define crates (parse-crates-stack lines-seq))
  (define moves (sequence-map parse-move-line lines-seq))
  (for ([m moves])
    (move-proc! crates m))
  (crates-top crates))

(define (day-5 input move-proc!)
  (call-with-input-file
   input
   (lambda~> in-lines (parse-line _ move-proc!))))

(day-5 "day_5.txt" move-v1!)
(day-5 "day_5.txt" move-v2!)