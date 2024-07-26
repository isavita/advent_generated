
#lang racket

(define (count-unique-yes-answers input)
  (define groups (string-split input "\n\n")) ; Split input into groups
  (define (count-yes group)
    (define answers (apply string-append (string-split group "\n"))) ; Combine all answers in the group
    (length (remove-duplicates (string->list answers)))) ; Count unique answers
  (apply + (map count-yes groups))) ; Sum counts of unique answers for each group

(define (main)
  (define input (file->string "input.txt")) ; Read input from file
  (define total-yes-count (count-unique-yes-answers input)) ; Count unique yes answers
  (printf "Total number of unique questions answered 'yes': ~a\n" total-yes-count)) ; Print result

(main)
