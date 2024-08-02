(define (react polymer)
  (define (react-helper stack chars)
    (cond
      ((null? chars) (reverse stack))
      ((and (not (null? stack))
            (let ((top (car stack))
                  (current (car chars)))
              (and (char-ci=? top current)
                   (not (char=? top current))))) 
       (react-helper (cdr stack) (cdr chars)))
      (else
       (react-helper (cons (car chars) stack) (cdr chars)))))
  (length (react-helper '() (string->list polymer))))

(define (main)
  (let ((input (with-input-from-file "input.txt"
                   (lambda () (read-line)))))
    (display (react input))
    (newline)))

(main)