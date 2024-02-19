#lang racket

;; Function to read frequency changes from a file
(define (read-frequency-changes file-path)
  (with-input-from-file file-path
    (lambda ()
      (let read-loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse lines)
              (read-loop (cons (string->number line) lines))))))))

;; Function to calculate the resulting frequency
(define (calculate-resulting-frequency changes)
  (foldl + 0 changes))

;; Main function to run the program
(define (main)
  (let ([changes (read-frequency-changes "input.txt")])
    (let ([resulting-frequency (calculate-resulting-frequency changes)])
      (printf "Resulting frequency: ~a\n" resulting-frequency))))

(main)
