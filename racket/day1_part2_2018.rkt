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

;; Function to find the first frequency reached twice
(define (find-first-frequency-reached-twice changes)
  (let loop ([frequencies (set)]
             [current-frequency 0]
             [changes-list changes])
    (cond
      [(set-member? frequencies current-frequency) current-frequency]
      [else
       (let ([next-frequency (+ current-frequency (first changes-list))]
             [next-changes-list (if (null? (rest changes-list))
                                    changes
                                    (rest changes-list))])
         (loop (set-add frequencies current-frequency)
               next-frequency
               next-changes-list))])))

;; Main function to run the program
(define (main)
  (let ([changes (read-frequency-changes "input.txt")])
    (let ([first-duplicate-frequency (find-first-frequency-reached-twice changes)])
      (printf "First frequency reached twice: ~a\n" first-duplicate-frequency))))

(main)

