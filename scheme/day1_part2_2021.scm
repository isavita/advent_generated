(define (read-input-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons (string->number line) lines))))))))

(define (count-increases vals)
  (define (helper prev-sum count i)
    (if (>= i (length vals))
        count
        (let* ((curr-sum (+ (list-ref vals (- i 2)) (list-ref vals (- i 1)) (list-ref vals i)))
               (new-count (if (> curr-sum prev-sum) (+ count 1) count)))
          (helper curr-sum new-count (+ i 1)))))
  (helper (+ (list-ref vals 0) (list-ref vals 1) (list-ref vals 2)) 0 3))

(define vals (read-input-file "input.txt"))
(display (count-increases vals))