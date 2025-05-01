
#lang racket

(require racket/file)
(require racket/set)

(define (parse-instruction instruction)
  (let loop ((i 0) (x 0) (y 0))
    (if (>= i (string-length instruction))
        (cons x y)
        (let ((c (string-ref instruction i)))
          (case c
            ((#\e) (loop (+ i 1) (+ x 1) y))
            ((#\w) (loop (+ i 1) (- x 1) y))
            ((#\s)
             (let ((next-c (string-ref instruction (+ i 1))))
               (case next-c
                 ((#\e) (loop (+ i 2) x (- y 1)))
                 ((#\w) (loop (+ i 2) (- x 1) (- y 1)))
                 (else (error "invalid instruction")))))
            ((#\n)
             (let ((next-c (string-ref instruction (+ i 1))))
               (case next-c
                 ((#\e) (loop (+ i 2) (+ x 1) (+ y 1)))
                 ((#\w) (loop (+ i 2) x (+ y 1)))
                 (else (error "invalid instruction")))))
            (else (error "invalid instruction")))))))

(define (main)
  (define instructions (file->lines "input.txt"))
  (let loop ((instructions instructions) (black-tiles (set)))
    (if (empty? instructions)
        (set-count black-tiles)
        (let* ((instruction (first instructions))
               (coords (parse-instruction instruction))
               (new-black-tiles (if (set-member? black-tiles coords)
                                    (set-remove black-tiles coords)
                                    (set-add black-tiles coords))))
          (loop (rest instructions) new-black-tiles)))))

(module+ main
  (print (main)))
