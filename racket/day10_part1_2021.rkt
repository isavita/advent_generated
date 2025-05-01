
#lang racket

(require racket/file)
(require racket/port)
(require racket/hash)

(define illegal-scores (hash #\) 3 #\] 57 #\} 1197 #\> 25137))
(define opener-for-closer (hash #\) #\( #\] #\[ #\} #\{ #\> #\<))

(define (process-line line-str)
  (let ((len (string-length line-str)))
    (let loop ((i 0) (stack '()))
      (if (= i len)
          0
          (let ((c (string-ref line-str i)))
            (case c
              [(#\( #\[ #\{ #\<)
               (loop (add1 i) (cons c stack))]
              [(#\) #\] #\} #\>)
               (if (empty? stack)
                   (hash-ref illegal-scores c)
                   (let ((opener (car stack)) (rest-stack (cdr stack)))
                     (if (eq? opener (hash-ref opener-for-closer c))
                         (loop (add1 i) rest-stack)
                         (hash-ref illegal-scores c))))]
              [else
               (loop (add1 i) stack)]))))))

(define (main)
  (let ((total-score
         (with-input-from-file "input.txt"
           (lambda ()
             (let ((lines (port->lines (current-input-port))))
               (apply + (map process-line lines)))))))
    (print total-score)))

(main)
