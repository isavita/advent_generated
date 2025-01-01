
#lang racket

(define (read-input filename)
  (file->string filename))

(define (increment-password password)
  (let* ([chars (string->list password)]
         [len (length chars)])
    (let loop ([i (- len 1)] [chars chars])
      (cond
        [(= i -1) (list->string chars)]
        [else
         (let ([new-char (integer->char (+ (char->integer (list-ref chars i)) 1))])
           (if (> (char->integer new-char) (char->integer #\z))
               (loop (- i 1) (list-set chars i #\a))
               (list->string (list-set chars i new-char))))]))))

(define (has-straight? password)
  (let* ([chars (string->list password)]
         [len (length chars)])
    (for/or ([i (in-range 0 (- len 2))])
      (and (= (+ (char->integer (list-ref chars i)) 1) (char->integer (list-ref chars (+ i 1))))
           (= (+ (char->integer (list-ref chars i)) 2) (char->integer (list-ref chars (+ i 2))))))))

(define (contains-invalid-letters? password)
  (for/or ([c (in-string password)])
    (or (char=? c #\i) (char=? c #\o) (char=? c #\l))))

(define (has-two-pairs? password)
  (let* ([chars (string->list password)]
         [len (length chars)])
    (let loop ([i 0] [count 0])
      (cond
        [(>= i (- len 1)) (>= count 2)]
        [(= (char->integer (list-ref chars i)) (char->integer (list-ref chars (+ i 1))))
         (loop (+ i 2) (+ count 1))]
        [else (loop (+ i 1) count)]))))

(define (valid-password? password)
  (and (has-straight? password)
       (not (contains-invalid-letters? password))
       (has-two-pairs? password)))

(define (find-next-password password)
  (let loop ([password password])
    (let ([next-password (increment-password password)])
      (if (valid-password? next-password)
          next-password
          (loop next-password)))))

(let* ([current-password (read-input "input.txt")]
       [first-new-password (find-next-password current-password)]
       [second-new-password (find-next-password first-new-password)])
  (displayln second-new-password))
