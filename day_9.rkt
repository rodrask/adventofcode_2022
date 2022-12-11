#lang racket
(require threading)

(struct pos (x y) #:transparent)

(define (sum pos1 pos2)
  (pos (+ (pos-x pos1) (pos-x pos2)) (+ (pos-y pos1) (pos-y pos2))))

(define (diff pos1 pos2)
  (pos (- (pos-x pos2) (pos-x pos1)) (- (pos-y pos2) (pos-y pos1))))

(define (init-rope [size 2])
  (make-vector size (pos 0 0)))

(define (rope-size r)
  (vector-length r))

(define (rope-tail r)
  (vector-ref r (sub1 (rope-size r))))

(define (check-rope r)
  (let ([list-rope (vector->list r)])
    (for/and ([f (drop-right list-rope 1)]
          [s (rest list-rope)]
          #:do [(define d (diff f s))])
      (set-member? allowed-diffs d))))

(define (is-tail? r pos)
  (= pos (sub1 (rope-size r))))

(define (get-rope r pos)
  (vector-ref r pos))

(define (set-rope! r pos v)
  (if (>= pos 0) (vector-set! r pos v) (vector-set! r (+ (rope-size r) pos) v)))

(define command-2-pos
  (hash "R"
        (pos 1 0)
        "UR"
        (pos 1 1)
        "U"
        (pos 0 1)
        "UL"
        (pos -1 1)
        "L"
        (pos -1 0)
        "DL"
        (pos -1 -1)
        "D"
        (pos 0 -1)
        "DR"
        (pos 1 -1)))
(define allowed-diffs (~> command-2-pos hash-values list->set (set-union _ (set (pos 0 0)))))
(define diff-2-command
  (hash (pos 2 0)
        "R"
        (pos -2 0)
        "L"
        (pos 0 2)
        "U"
        (pos 0 -2)
        "D"
        (pos 2 1)
        "UR"
        (pos 2 -1)
        "DR"
        (pos -2 1)
        "UL"
        (pos -2 -1)
        "DL"
        (pos 1 2)
        "UR"
        (pos -1 2)
        "UL"
        (pos 1 -2)
        "DR"
        (pos -1 -2)
        "DL"
        (pos 2 2)
        "UR"
        (pos 2 -2)
        "DR"
        (pos -2 -2)
        "DL"
        (pos -2 2)
        "UL"))

(define (need-next-move? df)
  (hash-has-key? diff-2-command df))

(define (diff-2-tail-move df)
  (hash-ref command-2-pos (hash-ref diff-2-command df)))

(define (single-step! rope direction)
  (define (segment-step! pos move)
    (let* ([segment-pos (get-rope rope pos)] [new-pos (sum segment-pos move)])
      (set-rope! rope pos new-pos)
      (when (not (is-tail? rope pos))
        (let ([diff-with-next (diff (get-rope rope (add1 pos)) new-pos)])
          (when (need-next-move? diff-with-next)
            (segment-step! (add1 pos) (diff-2-tail-move diff-with-next)))))))
  (segment-step! 0 (hash-ref command-2-pos direction)))

(define (apply-command! rope command callback)
  (let* ([com-parts (string-split command " ")]
         [direction (first com-parts)]
         [n-steps (~> com-parts second string->number)])
    (for ([_ (range n-steps)])
      (single-step! rope direction)
      (callback))))

(define (run-program! rope commands)
  (define tail-visited (mutable-set (pos 0 0)))
  (define (state-callback)
    (set-add! tail-visited (rope-tail rope)))
  (for ([command commands])
    (apply-command! rope command state-callback))
  (set-count tail-visited))

(define (day-9 input rope)
  (call-with-input-file input (lambda~> in-lines (run-program! rope _))))

(day-9 "day_9.txt" (init-rope))
(day-9 "day_9.txt" (init-rope 10))
