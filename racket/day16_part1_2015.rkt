
#lang racket

(require racket/match
         racket/string)

; --- Target Readings from MFCSAM ---
(define target-readings
  (hash 'children 3
        'cats 7
        'samoyeds 2
        'pomeranians 3
        'akitas 0
        'vizslas 0
        'goldfish 5
        'trees 3
        'cars 2
        'perfumes 1))

; --- Helper Function to Parse a Line ---
; Parses a line like "Sue 1: cars: 9, akitas: 3, goldfish: 0"
; Returns a list: (aunt-number properties-hash)
; Example return: (list 1 (hash 'cars 9 'akitas 3 'goldfish 0))
(define (parse-line line)
  (match (regexp-match #px"^Sue (\\d+): (.*)$" line)
    [(list _ aunt-num-str props-str)
     (let ([aunt-num (string->number aunt-num-str)]
           [properties (make-hash)])
       ; Split properties string and populate the hash
       (for ([prop-pair (string-split props-str ", ")])
         (match (string-split prop-pair ": ")
           [(list prop-name prop-val-str)
            (hash-set! properties
                       (string->symbol prop-name)
                       (string->number prop-val-str))]))
       (list aunt-num properties))]
    [_ (error 'parse-line "Invalid line format: ~a" line)]))

; --- Helper Function to Check if an Aunt Matches ---
; Checks if the known properties of an aunt match the target readings.
(define (aunt-matches? aunt-data target)
  (match-define (list _ aunt-properties) aunt-data)
  ; Iterate through all properties KNOWN for this aunt
  ; For each known property, check if its value matches the target value
  (for/and ([ (prop val) (in-hash aunt-properties) ])
    (equal? val (hash-ref target prop))))

; --- Main Function ---
(define (main)
  (define input-path "input.txt")

  ; Ensure input file exists
  (unless (file-exists? input-path)
    (eprintf "Error: Input file '~a' not found.\n" input-path)
    (exit 1))

  ; Read all aunts from the file
  (define all-aunts
    (with-input-from-file input-path
      (lambda ()
        (port->list read-line (current-input-port)))))

  ; Parse all aunts
  (define parsed-aunts (map parse-line all-aunts))

  ; Find the first aunt that matches all known criteria
  (define matching-aunt
    (findf (lambda (aunt) (aunt-matches? aunt target-readings))
           parsed-aunts))

  ; Print the result
  (cond
    [matching-aunt
     (printf "~a\n" (first matching-aunt))]
    [else
     (eprintf "Error: No matching Aunt Sue found.\n")
     (exit 1)]))

; --- Run the main function ---
(main)
