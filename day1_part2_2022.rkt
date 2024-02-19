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

;; Function to find the total Calories of the top three Elves
(define (find-top-three-elves-total inventories)
  (let* ([totals (map (lambda (inventory) (apply + inventory)) inventories)]
         [sorted-totals (sort totals >)])
    (apply + (take sorted-totals 3))))

;; Main function to run the program
(define (main)
  (let* ([lines (read-calories "input.txt")]
         [inventories (split-elves-inventories lines)]
         [top-three-total (find-top-three-elves-total inventories)])
    (printf "~a\n" top-three-total)))

(main)
