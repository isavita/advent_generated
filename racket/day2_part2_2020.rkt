#lang racket

(define (parse-line line)
  (define parts (string-split line ":"))
  (define policy (string-split (first parts) " "))
  (define range (string-split (first policy) "-"))
  (define low (string->number (first range)))
  (define high (string->number (second range)))
  (define letter (second policy))
  (define password (string-trim (second parts)))
  (list low high letter password))

(define (valid-password? low high letter password)
  (let* ((pos1 (substring password (- low 1) low))
         (pos2 (substring password (- high 1) high)))
    (or (and (equal? pos1 letter) (not (equal? pos2 letter)))
        (and (equal? pos2 letter) (not (equal? pos1 letter))))))

(define (count-valid-passwords filename)
  (define lines (file->lines filename))
  (define parsed (map parse-line lines))
  (define valid-count (filter (λ (p) (valid-password? (first p) (second p) (third p) (fourth p))) parsed))
  (length valid-count))

(define (main)
  (define result (count-valid-passwords "input.txt"))
  (displayln result))

(main)