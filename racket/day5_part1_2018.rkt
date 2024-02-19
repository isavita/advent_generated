#lang racket

(require racket/file)

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

; Main function to solve the challenge
(define (solve-challenge filepath)
  (define polymer (file->string filepath))
  (define reacted-polymer (react-polymer (string-trim polymer)))
  (string-length reacted-polymer))

; Run the program and print the result
(define result (solve-challenge "input.txt"))
(printf "~a\n" result)
