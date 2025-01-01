
#lang racket

(define (increment-password str)
  (define (increment-char char)
    (if (char=? char #\z) #\a (integer->char (+ 1 (char->integer char)))))
  (define (increment-rec str idx)
    (if (< idx 0)
        (cons #\a str)
        (let ((new-char (increment-char (list-ref str idx))))
          (if (char=? new-char #\a)
              (increment-rec (list-set str idx new-char) (- idx 1))
              (list-set str idx new-char)))))
  (list->string (increment-rec (string->list str) (- (string-length str) 1))))

(define (has-straight? str)
  (define (check-straight lst)
    (cond ((< (length lst) 3) #f)
          ((and (= (char->integer (list-ref lst 0)) (- (char->integer (list-ref lst 1)) 1))
                (= (char->integer (list-ref lst 1)) (- (char->integer (list-ref lst 2)) 1)))
           #t)
          (else (check-straight (cdr lst)))))
  (check-straight (string->list str)))

(define (has-forbidden-chars? str)
  (or (string-contains? str "i")
      (string-contains? str "o")
      (string-contains? str "l")))

(define (has-two-pairs? str)
  (define (find-pairs lst pairs)
    (cond ((< (length lst) 2) pairs)
          ((char=? (list-ref lst 0) (list-ref lst 1))
           (find-pairs (drop lst 2) (cons (list-ref lst 0) pairs)))
          (else (find-pairs (cdr lst) pairs))))
  (>= (length (remove-duplicates (find-pairs (string->list str) '()))) 2))

(define (is-valid-password? str)
  (and (has-straight? str)
       (not (has-forbidden-chars? str))
       (has-two-pairs? str)))

(define (find-next-password str)
  (let loop ((current-password str))
    (let ((next-password (increment-password current-password)))
      (if (is-valid-password? next-password)
          next-password
          (loop next-password)))))

(define (main)
  (let ((input-port (open-input-file "input.txt")))
    (let ((initial-password (read-line input-port)))
      (close-input-port input-port)
      (displayln (find-next-password initial-password)))))

(main)
