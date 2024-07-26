
#lang racket

(define (is-valid-triangle a b c)
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(define (count-valid-triangles numbers)
  (define valid-triangles 0)
  (for ([i (in-range (length (car numbers)))])
    (for ([j (in-range 0 (length numbers) 3)])
      (when (< j (+ 2 (length numbers)))
        (when (is-valid-triangle (list-ref (list-ref numbers j) i)
                                  (list-ref (list-ref numbers (+ j 1)) i)
                                  (list-ref (list-ref numbers (+ j 2)) i))
          (set! valid-triangles (+ valid-triangles 1))))))
  valid-triangles)

(define (read-input filename)
  (define lines (file->lines filename))
  (map (lambda (line) (map string->number (string-split line))) lines))

(define (main)
  (define numbers (read-input "input.txt"))
  (printf "~a\n" (count-valid-triangles numbers)))

(main)
