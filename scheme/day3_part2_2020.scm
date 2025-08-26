
; Read file and return list of strings (each string is a line)
(define (read-file filename)
  (let ((port (open-input-file filename)))
    (let loop ((line (read-line port)) (lines '()))
      (if (eof-object? line)
          (begin (close-input-port port) (reverse lines))
          (loop (read-line port) (cons line lines))))))

; Count trees encountered with given slope
(define (count-trees grid right down)
  (let ((height (length grid))
        (width (string-length (car grid))))
    (let loop ((row 0) (col 0) (count 0))
      (if (>= row height)
          count
          (let* ((current-row (list-ref grid row))
                 ( pos (modulo col width))
                 (char (string-ref current-row pos)))
            (if (char=? char #\#)
                (loop (+ row down) (+ col right) (+ count 1))
                (loop (+ row down) (+ col right) count)))))))

; Main function
(define (main)
  (let ((grid (read-file "input.txt")))
    (let ((slopes '((1 1) (3 1) (5 1) (7 1) (1 2)))
          (results '()))
      (for-each 
       (lambda (slope)
         (let ((right (car slope))
               (down (cadr slope)))
           (set! results (cons (count-trees grid right down) results))))
       slopes)
      (display "Trees encountered for each slope: ")
      (display (reverse results))
      (newline)
      (display "Product of all trees: ")
      (display (apply * (reverse results)))
      (newline))))

; Run the program
(main)
