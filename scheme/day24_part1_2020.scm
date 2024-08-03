(define (read-input filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define directions
  '(("e" . (1 0))
    ("se" . (0 1))
    ("sw" . (-1 1))
    ("w" . (-1 0))
    ("nw" . (0 -1))
    ("ne" . (1 -1))))

(define (move coord dir)
  (let ((delta (cdr (assoc dir directions))))
    (list (+ (car coord) (car delta))
          (+ (cadr coord) (cadr delta)))))

(define (toggle-tile black-tiles coord)
  (if (assoc coord black-tiles)
      (remove (lambda (x) (equal? (car x) coord)) black-tiles)
      (cons (cons coord #t) black-tiles)))

(define (count-black-tiles black-tiles)
  (length (filter (lambda (x) (cdr x)) black-tiles)))

(define (main)
  (let* ((lines (read-input "input.txt"))
         (black-tiles '()))
    (for-each
      (lambda (line)
        (let loop ((i 0) (coord '(0 0)))
          (if (< i (string-length line))
              (let* ((char (string-ref line i))
                     (dir (if (or (equal? char #\e) (equal? char #\w))
                               (string char)
                               (string char (string-ref line (+ i 1))))))
                (if (or (equal? char #\e) (equal? char #\w))
                    (loop (+ i 1) (move coord dir))
                    (loop (+ i 2) (move coord dir))))
              (set! black-tiles (toggle-tile black-tiles coord)))))
      lines)
    (display (count-black-tiles black-tiles))))

(main)