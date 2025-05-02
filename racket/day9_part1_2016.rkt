
#lang racket

(define marker-re #px"\\((\\d+)x(\\d+)\\)")

(define (get-decompressed-length s)
  (let loop ((index 0) (total-length 0))
    (if (>= index (string-length s))
        total-length
        (let ((match (regexp-match-positions marker-re s index)))
          (if match
              (let* ((marker-end (cdar match))
                     (len-match (cadr match))
                     (rep-match (caddr match))
                     (len (string->number (substring s (car len-match) (cdr len-match))))
                     (rep (string->number (substring s (car rep-match) (cdr rep-match)))))
                (loop (+ marker-end len)
                      (+ total-length (* len rep))))
              (loop (add1 index)
                    (add1 total-length)))))))

(define (main)
  (with-input-from-file "input.txt"
    (lambda ()
      (let ((input-text (read-line)))
        (let ((decompressed-length (get-decompressed-length input-text)))
          (printf "~a\n" decompressed-length))))))

(main)
