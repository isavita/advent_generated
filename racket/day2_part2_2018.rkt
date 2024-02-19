#lang racket

(define (read-file-lines filepath)
  (with-input-from-file filepath
    (lambda ()
      (let read-loop ((line (read-line))
                      (lines '()))
        (if (eof-object? line)
            (reverse lines)
            (read-loop (read-line) (cons line lines)))))))

(define (ids-differ-by-one id1 id2)
  (let loop ((chars1 (string->list id1))
             (chars2 (string->list id2))
             (differences 0))
    (cond ((or (null? chars1) (null? chars2)) (= differences 1))
          ((eq? (car chars1) (car chars2))
           (loop (cdr chars1) (cdr chars2) differences))
          ((< differences 1)
           (loop (cdr chars1) (cdr chars2) (add1 differences)))
          (else #f))))

(define (common-chars id1 id2)
  (let loop ((chars1 (string->list id1))
             (chars2 (string->list id2))
             (result '()))
    (cond ((or (null? chars1) (null? chars2)) (list->string (reverse result)))
          ((eq? (car chars1) (car chars2))
           (loop (cdr chars1) (cdr chars2) (cons (car chars1) result)))
          (else (loop (cdr chars1) (cdr chars2) result)))))

(define (find-and-print-common-chars ids)
  (let loop ((remaining-ids ids))
    (when (not (null? remaining-ids))
      (for-each (lambda (id2)
                  (when (ids-differ-by-one (car remaining-ids) id2)
                    (printf "~a\n" (common-chars (car remaining-ids) id2))
                    (exit)) ; Gracefully exit after finding and printing the result.
                )
                (cdr remaining-ids))
      (loop (cdr remaining-ids)))))

(define ids (read-file-lines "input.txt"))
(find-and-print-common-chars ids)
