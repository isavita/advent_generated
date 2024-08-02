(define (foldl proc init lst)
  (if (null? lst)
      init
      (foldl proc (proc init (car lst)) (cdr lst))))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (define changes '())
      (let loop ()
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse changes)
              (begin
                (set! changes (cons (string->number line) changes))
                (loop))))))))

(define (calculate-frequency changes)
  (foldl + 0 changes))

(define (main)
  (let ((changes (read-input "input.txt")))
    (display (calculate-frequency changes))
    (newline)))

(main)