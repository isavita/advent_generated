
#lang racket

(require racket/string
         racket/match)

;; Define the target profile from the MFCSAM
(define target-profile
  (hash 'children 3 'cats 7 'samoyeds 2 'pomeranians 3
        'akitas 0 'vizslas 0 'goldfish 5 'trees 3
        'cars 2 'perfumes 1))

;; Structure to hold information about each Aunt Sue
(struct aunt (num props) #:transparent) ; #:transparent helps with printing/debugging

;; Parses a single line from the input file into an aunt struct
;; Example line: "Sue 1: cars: 9, akitas: 3, goldfish: 0"
(define (parse-line line)
  (match (regexp-match #px"^Sue (\\d+): (.*)$" line)
    [(list _ num-str props-str)
     (let ([num (string->number num-str)]
           [props-hash (make-hash)])
       ; Parse the properties part: "cars: 9, akitas: 3, goldfish: 0"
       (for ([prop-pair (string-split props-str ", ")])
         (match (string-split prop-pair ": ")
           [(list prop-name val-str)
            (hash-set! props-hash (string->symbol prop-name) (string->number val-str))]
           [_ (error 'parse-line "Invalid property format: ~a" prop-pair)]))
       (aunt num props-hash))]
    [_ #f])) ; Return #f if the line doesn't match the expected format

;; Reads all aunts from the specified file
(define (load-aunts filename)
  (with-input-from-file filename
    (lambda ()
      (filter-map parse-line (port->lines (current-input-port))))))

;; --- Part 1 Matching Logic ---
;; Checks if an aunt's known properties match the target profile exactly.
(define (match-part1? aunt-data target)
  (andmap ; Returns #t iff the predicate is true for all items
   (lambda (prop)
     (equal? (hash-ref (aunt-props aunt-data) prop) ; Aunt's value
             (hash-ref target prop)))             ; Target value
   (hash-keys (aunt-props aunt-data)))) ; Iterate only over the properties this aunt has

;; --- Part 2 Matching Logic ---
;; Checks if an aunt's known properties match the target profile with retroencabulator rules.
(define (match-part2? aunt-data target)
  (andmap
   (lambda (prop)
     (let ([aunt-val (hash-ref (aunt-props aunt-data) prop)]
           [target-val (hash-ref target prop)])
       (case prop
         [(cats trees) (> aunt-val target-val)]         ; Greater than for cats and trees
         [(pomeranians goldfish) (< aunt-val target-val)] ; Fewer than for pomeranians and goldfish
         [else (equal? aunt-val target-val)])))         ; Exact match otherwise
   (hash-keys (aunt-props aunt-data))))

;; --- Main Entry Point ---
(define (main [filename "input.txt"])
  (let ([all-aunts (load-aunts filename)])

    ; --- Solve Part 1 ---
    (let ([matching-aunt-p1
           (findf (lambda (a) (match-part1? a target-profile)) ; findf stops at the first match
                  all-aunts)])
      (if matching-aunt-p1
          (printf "Part 1: ~a\n" (aunt-num matching-aunt-p1))
          (printf "Part 1: No matching Aunt found\n")))

    ; --- Solve Part 2 ---
    (let ([matching-aunt-p2
           (findf (lambda (a) (match-part2? a target-profile))
                  all-aunts)])
      (if matching-aunt-p2
          (printf "Part 2: ~a\n" (aunt-num matching-aunt-p2))
          (printf "Part 2: No matching Aunt found\n")))))


; Run the main function when the script is executed
(main)
