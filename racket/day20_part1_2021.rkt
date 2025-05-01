
#lang racket

(require racket/string)
(require racket/match)

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let* ([algorithm (string-trim (read-line))]
             [_ (read-line)]
             [image-lines (port->lines)]
             [image (map string->list image-lines)])
        (values algorithm image)))))

(define (enhance-image image algorithm times)
  (let loop ([img image] [k 0])
    (if (= k times)
        img
        (let ([flip (and (odd? k) (char=? (string-ref algorithm 0) #\#))])
          (loop (apply-algorithm img algorithm flip) (add1 k))))))

(define (apply-algorithm image algorithm flip)
  (let* ([old-h (length image)]
         [old-w (if (empty? image) 0 (length (first image)))]
         [new-h (+ old-h 2)]
         [new-w (+ old-w 2)])
    (for/list ([i (in-range new-h)])
      (for/list ([j (in-range new-w)])
        (let ([index (calculate-index (- i 1) (- j 1) image old-h old-w flip)])
          (string-ref algorithm index))))))

(define (calculate-index i j image old-h old-w flip)
  (let calculate-loop ([di -1] [dj -1] [current-index 0])
    (cond
      [(> di 1) current-index]
      [(> dj 1)
       (calculate-loop (add1 di) -1 current-index)]
      [else
       (let* ([ni (+ i di)]
              [nj (+ j dj)]
              [pixel-is-lit (cond
                              [(and (>= ni 0) (< ni old-h) (>= nj 0) (< nj old-w))
                               (char=? (list-ref (list-ref image ni) nj) #\#)]
                              [else flip])])
         (calculate-loop di (add1 dj) (if pixel-is-lit
                                          (bitwise-ior (arithmetic-shift current-index 1) 1)
                                          (arithmetic-shift current-index 1))))])))

(define (count-lit-pixels image)
  (for/sum ([row image])
    (for/sum ([char row])
      (if (char=? char #\#) 1 0))))

(define (main)
  (let-values ([(algorithm image) (read-input "input.txt")])
    (let ([enhanced-image (enhance-image image algorithm 2)])
      (displayln (count-lit-pixels enhanced-image)))))

(main)
