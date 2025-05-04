
#lang racket

(define opening-chars '(#\( #\[ #\{ #\<))
(define closing-chars '(#\) #\] #\} #\>))
(define pairings (hash #\) #\( #\] #\[ #\} #\{ #\> #\<))
(define completion-scores (hash #\) 1 #\] 2 #\} 3 #\> 4))
(define closing-for-opening (hash #\( #\) #\[ #\] #\{ #\} #\< #\>))

(define (calculate-completion-score stack)
  (let loop ((s stack) (score 0))
    (if (null? s)
        score
        (let* ((opening (car s))
               (closing (hash-ref closing-for-opening opening)))
          (loop (cdr s) (+ (* score 5) (hash-ref completion-scores closing)))))))

(define (process-line line)
  (let loop ((chars (string->list line)) (stack '()))
    (if (null? chars)
        (if (null? stack)
            #f
            (calculate-completion-score stack))
        (let ((char (car chars)))
          (cond
            ((member char opening-chars)
             (loop (cdr chars) (cons char stack)))
            ((member char closing-chars)
             (if (null? stack)
                 #f
                 (let ((expected-opening (hash-ref pairings char)))
                   (if (eq? (car stack) expected-opening)
                       (loop (cdr chars) (cdr stack))
                       #f
                       ))))
            (else
             (loop (cdr chars) stack)))))))

(module* main #f
  (define scores '())
  (with-input-from-file "input.txt"
    (lambda ()
      (let loop ()
        (let ((line (read-line)))
          (unless (eof-object? line)
            (let ((result (process-line (string-trim line))))
              (when result
                (set! scores (cons result scores)))
              (loop)))))))

  (define sorted-scores (sort scores <))
  (define num-scores (length sorted-scores))
  (define middle-index (quotient num-scores 2))
  (define middle-score (list-ref sorted-scores middle-index))

  (print middle-score))
