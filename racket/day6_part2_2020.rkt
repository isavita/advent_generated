
#lang racket

(require racket/base
         racket/string
         racket/list
         racket/set)

(module+ main
  (define groups (string-split (string-trim (file->string "input.txt")) "\n\n"))

  (define part-one-total
    (foldl
     (lambda (group total)
       (define unique-chars (list->set (string->list (string-join (string-split group "\n") ""))))
       (+ total (set-count unique-chars)))
     0 groups))

  (print part-one-total)

  (define part-two-total
    (foldl
     (lambda (group total)
       (define lines (string-split group "\n"))
       (define common-count
         (if (empty? lines)
             0
             (let ((first-set (list->set (string->list (car lines)))))
               (set-count
                (foldl
                 (lambda (line current-set)
                   (set-intersect current-set (list->set (string->list line))))
                 first-set
                 (cdr lines))))))
       (+ total common-count))
     0 groups))

  (print part-two-total))
