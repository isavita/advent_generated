
#lang racket

(define graph (make-hash))

(with-input-from-file "input.txt"
  (lambda ()
    (let loop ()
      (let ([line (read-line)])
        (unless (eof-object? line)
          (let* ([paths (string-split line "-")]
                 [from (first paths)]
                 [to (second paths)])
            (hash-set! graph from (cons to (hash-ref graph from '())))
            (hash-set! graph to (cons from (hash-ref graph to '()))))
          (loop))))))

(define (small-cave? cave-name)
  (and (> (string-length cave-name) 0)
       (char-lower-case? (string-ref cave-name 0))))

(define (dfs current-cave visited-small)
  (cond
    [(string=? current-cave "end")
     1]
    [else
     (apply +
            (for/list ([next-cave (hash-ref graph current-cave '())])
              (cond
                [(and (small-cave? next-cave) (set-member? visited-small next-cave))
                 0]
                [else
                 (let ([new-visited-small (if (small-cave? next-cave)
                                              (set-add visited-small next-cave)
                                              visited-small)])
                   (dfs next-cave new-visited-small))])))]))

(define initial-visited (if (small-cave? "start") (set "start") (set)))
(define total-paths (dfs "start" initial-visited))

(printf "~a~n" total-paths)
