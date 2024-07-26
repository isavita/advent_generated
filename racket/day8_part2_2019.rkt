
#lang racket

(define (min a b)
  (if (< a b) a b))

(define (decode-image)
  (define image-data (string-trim (file->string "input.txt")))
  (define width 25)
  (define height 6)
  (define layer-size (* width height))
  (define final-image (make-vector layer-size #\2))

  (for ([i (in-range 0 (string-length image-data) layer-size)])
    (define layer (substring image-data i (min (+ i layer-size) (string-length image-data))))
    (for ([j (in-range (string-length layer))])
      (when (eq? (vector-ref final-image j) #\2)
        (vector-set! final-image j (string-ref layer j)))))

  (displayln "Decoded image:")
  (for ([i (in-range height)])
    (for ([j (in-range width)])
      (define pixel (vector-ref final-image (+ (* i width) j)))
      (display (if (eq? pixel #\0) #\space #\#)))
    (newline)))

(decode-image)
