
#lang racket

(require racket/file)

(define (process-line line)
  (let ((chars (string->list line)))
    (let ((first-digit-char (findf char-numeric? chars))
          (last-digit-char (findf char-numeric? (reverse chars))))
      (if (and (char? first-digit-char) (char? last-digit-char))
          (let ((first-digit (string->number (string first-digit-char)))
                (last-digit (string->number (string last-digit-char))))
            (+ (* first-digit 10) last-digit))
          0))))

(module+ main
  (define filename "input.txt")
  (define lines (file->lines filename))
  (define values (map process-line lines))
  (define total (apply + values))
  (print total))

