#lang racket

(require racket/file racket/string)

; Function to check if two units react
(define (units-react? unit1 unit2)
  (and (= (abs (- (char->integer unit1) (char->integer unit2))) 32)
       (not (char=? unit1 unit2))))

; Function to perform the reactions in the polymer
(define (react-polymer polymer)
  (define (react-helper poly result)
    (cond
      [(empty? poly) (list->string (reverse result))]
      [(and (not (empty? result))
            (units-react? (first poly) (first result)))
       (react-helper (rest poly) (rest result))]
      [else
       (react-helper (rest poly) (cons (first poly) result))]))
  (react-helper (string->list polymer) '()))

; Removes all instances of a unit type from the polymer
(define (remove-unit polymer unit)
  (list->string (filter (lambda (char)
                          (and (not (char=? char unit))
                               (not (char=? char (char-upcase unit)))
                               (not (char=? char (char-downcase unit)))))
                        (string->list polymer))))

; Adjusted function to read and trim the polymer string from the file
(define (read-and-trim-polymer filepath)
  (string-trim (file->string filepath)))

; Adjusted main function to use the trimmed polymer for Part Two
(define (solve-part-two filepath)
  (define polymer (read-and-trim-polymer filepath)) ; Use the trimmed polymer
  (define units (string->list "abcdefghijklmnopqrstuvwxyz"))
  (define lengths (map (lambda (unit)
                         (string-length (react-polymer (remove-unit polymer unit))))
                       units))
  (apply min lengths))

; Run the program and print the result for Part Two with trimming
(define result-part-two (solve-part-two "input.txt"))
(printf "~a\n" result-part-two)

