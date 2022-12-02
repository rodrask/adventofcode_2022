#lang racket
(require threading)

(struct round (opp-move my-move))

(define (select-my-move opp-move end)
  (match (cons opp-move end)
    [(cons X 'Draw) X]
    [(cons 'Rock 'Lose) 'Scissors]
    [(cons 'Rock 'Win) 'Paper]
    [(cons 'Paper 'Lose) 'Rock]
    [(cons 'Paper 'Win) 'Scissors]
    [(cons 'Scissors 'Lose) 'Paper]
    [(cons 'Scissors 'Win) 'Rock]))

(define get-char-2-move (curry hash-ref (hash "A" 'Rock "X" 'Rock 
						  "B" 'Paper "Y" 'Paper 
						  "C" 'Scissors "Z" 'Scissors)))

(define get-char-2-end (curry hash-ref (hash "X" 'Lose "Y" 'Draw "Z" 'Win)))

(define get-move-score-hash (curry hash-ref (hash 'Rock 1 'Paper 2 'Scissors 3)))

(define (move-score r) (~> r round-my-move get-move-score-hash))

(define (round-score r)
  (match r
    [(round 'Rock 'Scissors) 0]
    [(round 'Paper 'Rock) 0]
    [(round 'Scissors 'Paper) 0]
    [(round A A) 3]
    [_ 6]))

(define (total-score r)
  (+ (move-score r) (round-score r)))

(define (parse-line-1 line)

  (~> line string-trim (string-split _ " ") (map get-char-2-move _) (apply round _) total-score))

(define (parse-line-2 line)
  (let* ([items (~> line string-trim (string-split _ " "))]
         [opp-move (~> items first get-char-2-move)]
         [end (~> items second get-char-2-end)]
         [my-move (select-my-move opp-move end)])
    (total-score (round opp-move my-move))))

(define (day-2 input parse-proc)
  (call-with-input-file
   input
   (lambda (in-port) (sequence-fold + 0 (sequence-map parse-proc (in-lines in-port))))))

(day-2 "day_2.txt" parse-line-1)
(day-2 "day_2.txt" parse-line-2)
