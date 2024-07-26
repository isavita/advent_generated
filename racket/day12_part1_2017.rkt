
#lang racket

(define (read-input filename)
  (define in (open-input-file filename))
  (define adj (make-hash))
  (define (add-edge from to)
    (hash-set! adj from (cons to (hash-ref adj from '())))
    (hash-set! adj to (cons from (hash-ref adj to '()))))
  (for ([line (in-lines in)])
    (define parts (string-split line " <-> "))
    (define from (string->number (first parts)))
    (for ([to (string-split (second parts) ", ")])
      (add-edge from (string->number to))))
  (close-input-port in)
  adj)

(define (dfs node adj visited)
  (unless (hash-ref visited node #f)
    (hash-set! visited node #t)
    (for ([neighbor (hash-ref adj node '())])
      (dfs neighbor adj visited))))

(define (count-connected-nodes)
  (define adj (read-input "input.txt"))
  (define visited (make-hash))
  (dfs 0 adj visited)
  (hash-count visited))

(displayln (count-connected-nodes))
