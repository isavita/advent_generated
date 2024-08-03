(define (react polymer)
  (let loop ((stack '()) (remaining polymer))
    (cond
      ((null? remaining) (length stack))
      ((and (not (null? stack))
            (let ((top (car stack))
                  (current (car remaining)))
              (and (char=? (char-downcase top) (char-downcase current))
                   (not (char=? top current))))) 
       (loop (cdr stack) (cdr remaining)))  ; React
      (else
       (loop (cons (car remaining) stack) (cdr remaining))))))

(define (remove-units polymer unit)
  (filter (lambda (c) (not (char=? (char-downcase c) (char-downcase unit)))) polymer))

(define (remove-duplicates lst)
  (let loop ((lst lst) (result '()))
    (cond
      ((null? lst) (reverse result))
      ((member (car lst) result) (loop (cdr lst) result))
      (else (loop (cdr lst) (cons (car lst) result))))))

(define (shortest-polymer-length polymer)
  (let ((units (remove-duplicates (map char-downcase polymer))))
    (apply min (map (lambda (u) (react (remove-units polymer u))) units))))

(define (main)
  (let ((input (with-input-from-file "input.txt"
                  (lambda () (read-line)))))
    (let ((length (shortest-polymer-length (string->list input))))
      (display length)
      (newline))))

(main)