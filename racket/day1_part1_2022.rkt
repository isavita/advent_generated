#lang racket

;; Function to read Elves' calories inventory from a file
(define (read-calories file-path)
  (with-input-from-file file-path
    (lambda ()
      (let read-loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse lines)
              (read-loop (cons line lines))))))))

;; Function to split the inventory into separate Elves' inventories
(define (split-elves-inventories lines)
  (let loop ([lines lines] [current-inventory '()] [inventories '()])
    (cond
      [(null? lines) (reverse (cons (reverse current-inventory) inventories))]
      [(string=? (car lines) "") (loop (cdr lines) '() (cons (reverse current-inventory) inventories))]
      [else (loop (cdr lines) (cons (string->number (car lines)) current-inventory) inventories)])))

;; Function to find the Elf carrying the most calories
(define (find-max-calories-elf inventories)
  (apply max (map (lambda (inventory) (apply + inventory)) inventories)))

;; Main function to run the program
(define (main)
  (let* ([lines (read-calories "input.txt")]
         [inventories (split-elves-inventories lines)]
         [max-calories (find-max-calories-elf inventories)])
    (printf "~a\n" max-calories)))

(main)

