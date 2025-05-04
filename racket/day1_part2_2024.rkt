
#lang racket

(module main racket
  (define data
    (with-input-from-file "input.txt"
      (lambda ()
        (for/list ([line (in-lines)])
          (let ((parts (string-split line)))
            (list (string->number (first parts))
                  (string->number (second parts))))))))

  (define ids (map first data))
  (define similarities (map second data))

  (define sim-counts (make-hash))
  (for ([sim similarities])
    (hash-update! sim-counts sim add1 0))

  (define total
    (for/sum ([id ids])
      (* id (hash-ref sim-counts id 0))))

  (print total))
