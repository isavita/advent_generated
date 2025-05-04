
#lang racket

(define (read-input filename)
  (file->lines filename))

(define (count-bits-at-index values index)
  (let loop ((vals values) (zeros 0) (ones 0))
    (if (empty? vals)
        (cons zeros ones)
        (let ((s (first vals)))
          (if (char=? (string-ref s index) #\0)
              (loop (rest vals) (add1 zeros) ones)
              (loop (rest vals) zeros (add1 ones)))))))

(define (filter-by-bit values index keep-char)
  (filter (lambda (s) (char=? (string-ref s index) keep-char))
          values))

(define (filter-values values criteria index)
  (if (= (length values) 1)
      (first values)
      (let* ((bit-count (count-bits-at-index values index))
             (zeros (car bit-count))
             (ones (cdr bit-count))
             (keep-char (criteria zeros ones)))
        (filter-values (filter-by-bit values index keep-char)
                       criteria
                       (+ index 1)))))

(define (main)
  (let* ((values (read-input "input.txt"))

         (oxygen-criteria (lambda (zeros ones)
                            (if (>= ones zeros) #\1 #\0)))
         (co2-criteria (lambda (zeros ones)
                         (if (<= zeros ones) #\0 #\1)))

         (oxygen-rating-str (filter-values values oxygen-criteria 0))
         (oxygen-rating-int (string->number oxygen-rating-str 2))

         (co2-rating-str (filter-values values co2-criteria 0))
         (co2-rating-int (string->number co2-rating-str 2)))

    (print (* oxygen-rating-int co2-rating-int))))

(main)
