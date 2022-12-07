#lang racket
(require threading)

(struct fs-node (name parent))
(struct file fs-node (size))
(struct dir fs-node ([content #:mutable]) #:transparent)

(define (fullname parent name)
  (format "~a/~a" (fs-node-name parent) name))

(define (go-up! node-box)
  (set-box! node-box (~> node-box unbox fs-node-parent)))

(define (go-into! node-box dirname)
  (define (find-proc node)
    (equal? dirname (fs-node-name node)))
  (set-box! node-box (~> node-box unbox dir-content (findf find-proc _))))

(define (add-node! dir node)
  (set-dir-content! dir (cons node (dir-content dir))))

(define (build-fs! lines)
  (define root (dir "/" null '()))
  (define current-node (box root))
  (for ([command lines])
    (match (~> command string-trim (string-split _ " "))
      [(list "$" "cd" "/") (set-box! current-node root)]
      [(list "$" "cd" "..") (go-up! current-node)]
      [(list "$" "cd" dirname) (go-into! current-node (fullname (unbox current-node) dirname))]
      [(list "$" "ls") (void)]
      [(list "dir" dirname)
       (let* ([parent (unbox current-node)] [full (fullname parent dirname)])
         (add-node! parent (dir full parent '())))]
      [(list fsize filename)
       (add-node! (unbox current-node) (file filename (unbox current-node) (string->number fsize)))]))
  root)

(define (traverse-fs root)
  (define result (make-hash))
  (define (callback dirname size)
    (hash-set! result dirname size))

  (define (recursive-size node callback)
    (match node
      [(file _ _ size) size]
      [(dir name _ content)
       (let ([dirsize (foldl + 0 (map (curryr recursive-size callback) content))])
         (callback name dirsize)
         dirsize)]))
  (recursive-size root callback)
  result)

(define (dir-2-delete fs-stat [total-space 70000000] [free-space 30000000])
  (let* ([unused-space (- total-space (hash-ref fs-stat "/"))]
         [required-space (- free-space unused-space)])
    (~> fs-stat hash-values (filter (curryr >= required-space) _) (apply min _))))

(define (day-7-part-1 input)
  (call-with-input-file
   input
   (lambda~> in-lines build-fs! traverse-fs hash-values (filter (curryr <= 100000) _) (foldl + 0 _))))

(define (day-7-part-2 input)
  (call-with-input-file input (lambda~> in-lines build-fs! traverse-fs dir-2-delete)))

(day-7-part-1 "day_7.txt")
(day-7-part-2 "day_7.txt")
