
;; Memory Maneuver - Part 1
;; Reads input from input.txt and prints the sum of all metadata entries.

;; Read all numbers from the given file (tokens separated by whitespace)
(define (read-numbers filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((acc '()))
        (let ((x (read)))
          (if (eof-object? x)
              (reverse acc)
              (loop (cons x acc))))))))

;; Parse a node starting at position 'pos' in the list 'nums'.
;; Returns a pair (sum-of-metadata . new-position)
(define (parse-node nums pos)
  (let ((child-count (list-ref nums pos))
        (metadata-count (list-ref nums (+ pos 1))))
    (let loop ((i 0) (cur (+ pos 2)) (sum 0))
      (if (= i child-count)
          ;; Process this node's metadata
          (let loop2 ((j 0) (mpos cur) (sum2 sum))
            (if (= j metadata-count)
                (cons sum2 mpos)
                (loop2 (+ j 1) (+ mpos 1) (+ sum2 (list-ref nums mpos)))))
          ;; Process next child
          (let ((child (parse-node nums cur)))
            (loop (+ i 1) (cdr child) (+ sum (car child))))))))

;; Main entry point
(define (main)
  (let* ((nums (read-numbers "input.txt"))
         (result (parse-node nums 0)))
    (display (car result))
    (newline)))

(main)
