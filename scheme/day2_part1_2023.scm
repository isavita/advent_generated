#!/usr/bin/env scheme

(define targets '(("red" . 12) ("green" . 13) ("blue" . 14)))

(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (string-index-from str start ch)
  (let loop ((i start))
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) ch) i)
          (else (loop (+ i 1))))))

(define (string-index str ch)
  (string-index-from str 0 ch))

(define (string-split str ch)
  (let loop ((start 0) (res '()))
    (let ((pos (string-index-from str start ch)))
      (if pos
          (loop (+ pos 1) (cons (substring str start pos) res))
          (reverse (cons (substring str start (string-length str)) res))))))

(define (trim str)
  (let ((len (string-length str)))
    (let loop-start ((i 0))
      (if (or (= i len) (not (char-whitespace? (string-ref str i))))
          (let loop-end ((j (- len 1)))
            (if (or (< j i) (not (char-whitespace? (string-ref str j))))
                (substring str i (+ j 1))
                (loop-end (- j 1))))
          (loop-start (+ i 1))))))

(define (game-id line)
  (let* ((parts (string-split line #\:))
         (header (car parts))
         (id-str (cadr (string-split header #\space))))
    (string->number id-str)))

(define (game-possible? line)
  (let* ((id (game-id line))
         (after (cadr (string-split line #\:)))
         (rounds (string-split after #\;)))
    (let loop-rounds ((r rounds))
      (if (null? r)
          id
          (let* ((tokens (string-split (car r) #\,)))
            (if (let loop-tokens ((t tokens))
                  (if (null? t) #t
                      (let* ((tok (trim (car t)))
                             (parts (string-split tok #\space))
                             (count (string->number (car parts)))
                             (color (cadr parts))
                             (limit (cdr (assoc color targets))))
                        (if (> count limit)
                            #f
                            (loop-tokens (cdr t))))))
                (loop-rounds (cdr r))
                0))))))

(define (main)
  (let* ((lines (read-lines "input.txt"))
         (ids (map game-possible? lines))
         (total (apply + ids)))
    (display total)
    (newline)))

(main)
