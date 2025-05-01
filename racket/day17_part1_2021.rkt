
#lang racket
(require racket/string)

(define (main)
  (define line (with-input-from-file "input.txt" read-line))
  (define parts (string-split line ": "))
  (define ranges-str (second parts))
  (define range-parts (string-split ranges-str ", "))
  (define x-range-str (substring (first range-parts) 2))
  (define y-range-str (substring (second range-parts) 2))
  (define x-bounds-str (string-split x-range-str ".."))
  (define y-bounds-str (string-split y-range-str ".."))
  (define x-min (string->number (first x-bounds-str)))
  (define x-max (string->number (second x-bounds-str)))
  (define y-min (string->number (first y-bounds-str)))
  (define y-max (string->number (second y-bounds-str)))

  (define max-y-vel (- 0 y-min 1))
  (define max-height (/ (* max-y-vel (+ max-y-vel 1)) 2))

  (displayln max-height))

(main)
