(define (find-product-of-entries filename)
  (define (read-numbers filename)
    (with-input-from-file filename
      (lambda ()
        (let loop ((numbers '()))
          (let ((line (read-line)))
            (if (eof-object? line)
                (reverse numbers)
                (loop (cons (string->number line) numbers))))))))

  (define (find-two-sum numbers target)
    (let loop ((nums numbers) (seen '()))
      (cond
        ((null? nums) #f)
        (else
         (let ((current (car nums))
               (needed (- target (car nums))))
           (if (member needed seen)
               (* current needed)
               (loop (cdr nums) (cons current seen))))))))

  (let ((numbers (read-numbers filename)))
    (find-two-sum numbers 2020)))

(display (find-product-of-entries "input.txt"))