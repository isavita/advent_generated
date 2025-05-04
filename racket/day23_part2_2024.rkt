
#lang racket

(require racket/set)
(require racket/hash)
(require racket/string)

(define (solve)
  (define graph (make-hash))
  (define nodes (set))

  (with-input-from-file "input.txt"
    (lambda ()
      (let loop ()
        (let ([line (read-line)])
          (unless (eof-object? line)
            (let ([parts (string-split line "-")])
              (let ([a (first parts)]
                    [b (second parts)])
                (hash-update! graph a (lambda (neighbors) (set-add neighbors b)) (set))
                (hash-update! graph b (lambda (neighbors) (set-add neighbors a)) (set))
                (set! nodes (set-add nodes a))
                (set! nodes (set-add nodes b))))
            (loop))))))

  (define best-clique (box '()))

  (define (bron-kerbosch r p x)
    (when (and (set-empty? p) (set-empty? x))
      (let ([current-clique-list (set->list r)])
        (when (> (length current-clique-list) (length (unbox best-clique)))
          (set-box! best-clique current-clique-list))))

    (let loop ([current-p p] [current-x x])
      (unless (set-empty? current-p)
        (let ([v (first (set->list current-p))])
          (let ([neighbors (hash-ref graph v (set))])
            (bron-kerbosch (set-add r v)
                           (set-intersect current-p neighbors)
                           (set-intersect current-x neighbors)))
          (loop (set-remove current-p v) (set-add current-x v))))))

  (bron-kerbosch (set) nodes (set))

  (let ([result (unbox best-clique)])
    (let ([sorted-result (sort result string<?)])
      (display (string-join sorted-result ","))
      (newline))))

(module+ main
  (solve))
