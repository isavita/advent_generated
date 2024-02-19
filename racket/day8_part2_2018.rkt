
#lang racket

(define (read-input filename)
  (define file (open-input-file filename))
  (define line (read-line file))
  (close-input-port file)
  (map string->number (string-split line " ")))

(define (parse-tree data index)
  (define child-count (list-ref data index))
  (define meta-count (list-ref data (+ index 1)))
  (set! index (+ index 2))

  (define child-values (make-vector child-count))
  (define (parse-child i)
    (define-values (child-value new-index) (parse-tree data index))
    (vector-set! child-values i child-value)
    (set! index new-index))
  (for ([i (in-range child-count)])
    (parse-child i))

  (define value 0)
  (if (= child-count 0)
      (for ([i (in-range meta-count)])
        (set! value (+ value (list-ref data (+ index i)))))
      (for ([i (in-range meta-count)])
        (define metadata (list-ref data (+ index i)))
        (when (and (<= metadata child-count) (> metadata 0))
          (set! value (+ value (vector-ref child-values (- metadata 1)))))))
  (set! index (+ index meta-count))
  (values value index))

(define numbers (read-input "input.txt"))
(define-values (value _) (parse-tree numbers 0))
(printf "~a\n" value)
