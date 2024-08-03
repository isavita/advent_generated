(define (string-empty? str)
  (equal? str ""))

(define (read-calories filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line))
                 (calories '())
                 (current-sum 0))
        (cond
          ((eof-object? line) (if (> current-sum 0) (cons current-sum calories) calories))
          ((string? line)
           (if (string-empty? line)
               (loop (read-line) (if (> current-sum 0) (cons current-sum calories) calories) 0)
               (loop (read-line) calories (+ current-sum (string->number line))))))))))
  
(define (max-calories calorie-list)
  (apply max calorie-list))

(define (main)
  (let* ((calories (read-calories "input.txt"))
         (max-carrying-elf (max-calories calories)))
    (display max-carrying-elf)
    (newline)))

(main)