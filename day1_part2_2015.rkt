#lang racket

;; Function to read the instructions from a file
(define (read-instructions file-path)
  (with-input-from-file file-path
    (lambda () (read-line))))

;; Function to find the position of the first character that takes Santa to the basement
(define (find-basement-entry-position instructions)
  (let loop ([pos 1] [floor 0] [chars (string->list instructions)])
    (cond
      [(null? chars) #f]  ; If there are no more characters, return false (should not happen in this task)
      [(< floor 0) (sub1 pos)] ; If Santa has reached the basement, return the position
      [else
       (loop (add1 pos)
             (case (car chars)
               [(#\() (add1 floor)]
               [(#\)) (sub1 floor)])
             (cdr chars))])))

;; Main function to run the program
(define (main)
  (let ([instructions (read-instructions "input.txt")])
    (define position (find-basement-entry-position instructions))
    (when position
      (printf "Position of the character that causes Santa to first enter the basement: ~a\n" position))))

(main)
