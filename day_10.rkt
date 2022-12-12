#lang racket
(require threading)
(require math)

(struct cpu (cycles X) #:mutable)

(define (init-cpu)
  (cpu 0 1))

(define (inc-cycle! cpu cycle-callback)
  (set-cpu-cycles! cpu (add1 (cpu-cycles cpu)))
  (cycle-callback cpu))

(define (noop! cpu cycle-callback)
  (inc-cycle! cpu cycle-callback))

(define (addx! cpu v cycle-callback)
  (inc-cycle! cpu cycle-callback)
  (inc-cycle! cpu cycle-callback)
  (set-cpu-X! cpu (+ (cpu-X cpu) v)))

(define (log-signal-strength! cpu strength-box)
  (let ([cycle (cpu-cycles cpu)])
    (when (= 20 (modulo cycle 40))
      (set-box! strength-box (+ (* cycle (cpu-X cpu)) (unbox strength-box))))))

(define (log-crt-data! cpu crt-vec)
  (let* ([pos (sub1 (cpu-cycles cpu))] [crt-pos (modulo pos 40)] [sprite-pos (cpu-X cpu)])
    (when (<= (abs (- crt-pos sprite-pos)) 1)
      (vector-set! crt-vec pos #\#))))

(define (show-crt crt-vec)
  (let ([arr (~> crt-vec vector->array (array-reshape _ #(6 40)))])
    (for ([row (in-array-axis arr 0)])
      (~> row array->list (apply string _) println))))

(define (run-program! lines cpu-callback)
  (define cpu (init-cpu))
  (define (noop-call!)
    (noop! cpu cpu-callback))
  (define (addx-call! v)
    (addx! cpu v cpu-callback))
  (for ([line lines])
    (match (string-split line " ")
      [(list "noop") (noop-call!)]
      [(list "addx" n) (addx-call! (string->number n))])))

(define (day-10-1 input)
  (define strength (box 0))
  (call-with-input-file input
                        (lambda~> in-lines (run-program! _ (curryr log-signal-strength! strength))))
  (unbox strength))

(define (day-10-2 input)
  (define crt-vec (make-vector 240 #\ ))
  (call-with-input-file input (lambda~> in-lines (run-program! _ (curryr log-crt-data! crt-vec))))
  (show-crt crt-vec))

(day-10-1 "day_10.txt")
(day-10-2 "day_10.txt")
