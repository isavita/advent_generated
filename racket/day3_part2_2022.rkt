
#lang racket

(require racket/string)
(require racket/set)
(require racket/runtime-path)

(define-runtime-path input-file "input.txt")

(define (read-lines filename)
  (with-input-from-file filename
    (lambda () (port->lines (current-input-port)))))

(define (calculate-priority c)
  (if (char-lower-case? c)
      (+ (- (char->integer c) (char->integer #\a)) 1)
      (+ (- (char->integer c) (char->integer #\A)) 27)))

(define (string->set s)
  (list->set (string->list s)))

(define (solve-part-one lines)
  (for/sum ([line (in-list lines)])
    (let* ([n (string-length line)]
           [mid (/ n 2)]
           [s1 (substring line 0 mid)]
           [s2 (substring line mid)])
      (let ([set1 (string->set s1)]
            [set2 (string->set s2)])
        (for/sum ([c (in-set (set-intersect set1 set2))])
          (calculate-priority c))))))

(define (solve-part-two lines)
  (for/sum ([i (in-range 0 (length lines) 3)])
    (let ([s1 (list-ref lines i)]
          [s2 (list-ref lines (+ i 1))]
          [s3 (list-ref lines (+ i 2))])
      (let ([set1 (string->set s1)]
            [set2 (string->set s2)]
            [set3 (string->set s3)])
        (for/sum ([c (in-set (set-intersect set1 set2 set3))])
          (calculate-priority c))))))

(define data (read-lines input-file))

(displayln (solve-part-one data))
(displayln (solve-part-two data))
