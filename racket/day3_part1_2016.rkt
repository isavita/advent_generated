
#lang racket

(define (valid-triangle? a b c)
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(define (count-possible-triangles triangles)
  (let loop ([triangles triangles] [count 0])
    (cond
      [(empty? triangles) count]
      [(valid-triangle? (first (first triangles)) (second (first triangles)) (third (first triangles)))
       (loop (rest triangles) (+ count 1))]
      [else
       (loop (rest triangles) count)])))

(define input (file->lines "input.txt"))
(define triangles (map (λ (line) (map string->number (regexp-match* #px"\\d+" line))) input))

(display (count-possible-triangles triangles))
