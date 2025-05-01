
#lang racket

(require racket/string racket/vector racket/file)

(define (vector-index-of vec elem)
  (let loop ([i 0])
    (when (< i (vector-length vec))
      (if (equal? (vector-ref vec i) elem)
          i
          (loop (+ i 1))))))

(define (spin programs x)
  (let* ([n (vector-length programs)]
         [split-point (- n x)])
    (vector-append (vector-drop programs split-point)
                   (vector-take programs split-point))))

(define (exchange programs a b)
  (let* ([n (vector-length programs)]
         [new-programs (make-vector n)])
    (for ([i (in-range n)])
      (vector-set! new-programs i
                   (cond [(= i a) (vector-ref programs b)]
                         [(= i b) (vector-ref programs a)]
                         [else    (vector-ref programs i)])))
    new-programs))

(define (partner programs char-a char-b)
  (let ([a (vector-index-of programs char-a)]
        [b (vector-index-of programs char-b)])
    (exchange programs a b)))

(define (main)
  (define programs (vector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p))

  (define moves-str
    (with-input-from-file "input.txt"
      (lambda () (read-line))))

  (define moves (string-split moves-str ","))

  (define current-programs programs)

  (for ([move-str moves])
    (match (string-ref move-str 0)
      [#\s (let ([x (string->number (substring move-str 1))])
             (set! current-programs (spin current-programs x)))]
      [#\x (let* ([parts (string-split (substring move-str 1) "/")]
                  [a (string->number (first parts))]
                  [b (string->number (second parts))])
             (set! current-programs (exchange current-programs a b)))]
      [#\p (let* ([parts (string-split (substring move-str 1) "/")]
                  [char-a (string-ref (first parts) 0)]
                  [char-b (string-ref (second parts) 0)])
             (set! current-programs (partner current-programs char-a char-b)))]))

  (print (list->string (vector->list current-programs))))

(main)
