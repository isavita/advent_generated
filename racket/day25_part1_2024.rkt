
#lang racket

;; Constants
(define SCHEMATIC-HEIGHT 7)
(define SCHEMATIC-WIDTH 5)
;; Rows 1 through 5 (0-indexed) contain the varying part of the pin/key.
;; Row 0 is always # for locks, . for keys.
;; Row 6 is always . for locks, # for keys.
(define INTERNAL-ROWS (range 1 6))

;; The problem example implies that two heights overlap if their sum is > 5.
;; lock-h + key-h <= 5 means they fit in that column.
(define FIT-THRESHOLD 5)

;; schematic->heights : (listof string) -> (listof integer)
;; Converts a 7-line schematic (list of strings) into a list of 5 heights.
;; Based on reverse-engineering the examples, the height for a column
;; is the count of '#' characters in that column across rows 1 through 5.
(define (schematic->heights schematic-lines)
  ;; Extract the internal rows (indices 1 to 5)
  (define internal-schematic-rows (take (rest schematic-lines) (length INTERNAL-ROWS)))

  ;; For each column, count the '#' characters in the internal rows
  (for/list ([col-index (range SCHEMATIC-WIDTH)])
    (count (lambda (row-string)
             (char=? (string-ref row-string col-index) #\#))
           internal-schematic-rows)))

;; fits? : (listof integer) (listof integer) -> boolean
;; Checks if a lock-heights list fits with a key-heights list.
;; They fit if the sum of heights for each column is <= FIT-THRESHOLD.
(define (fits? lock-heights key-heights)
  (andmap (lambda (lk hk)
            (<= (+ lk hk) FIT-THRESHOLD))
          lock-heights
          key-heights))

;; main : -> void
;; Reads schematics from input.txt, processes them, and prints the count
;; of unique lock/key pairs that fit.
(define (main)
  (with-input-from-file "input.txt"
    (lambda ()
      ;; Read all schematics from the input file
      (let loop ((current-schematic-lines '())
                 (all-raw-schematics '()))
        (define line (read-line))
        (cond
          ;; End of input: Process the last schematic block (if any) and then all collected blocks
          ((eof-object? line)
           (let ((final-schematics (if (and (not (empty? current-schematic-lines))
                                             (= (length current-schematic-lines) SCHEMATIC-HEIGHT))
                                         (cons (reverse current-schematic-lines) all-raw-schematics)
                                         all-raw-schematics)))

             ;; Separate raw schematics into locks and keys
             ;; Locks start with "#####", keys start with "....." (based on problem description and examples)
             (let-values (((locks-raw keys-raw)
                           (for/fold ((locks '()) (keys '()))
                                     ([sch final-schematics])
                             (if (string=? (first sch) "#####") ; Check if it's a lock
                                 (values (cons sch locks) keys)
                                 (values locks (cons sch keys)))))) ; Assume it's a key otherwise

               ;; Convert raw schematics into lists of heights
               (define lock-height-lists (map schematic->heights locks-raw))
               (define key-height-lists (map schematic->heights keys-raw))

               ;; Count the number of fitting lock/key pairs
               (let ((fit-count 0))
                 (for ([lock-hs lock-height-lists])
                   (for ([key-hs key-height-lists])
                     (when (fits? lock-hs key-hs)
                       (set! fit-count (add1 fit-count)))))
                 (displayln fit-count)))))

          ;; Blank line: Marks the end of a schematic block
          ((string=? line "")
           (if (and (not (empty? current-schematic-lines)) (= (length current-schematic-lines) SCHEMATIC-HEIGHT))
               ;; Process the complete schematic block and start a new one
               (loop '() (cons (reverse current-schematic-lines) all-raw-schematics))
               ;; Ignore blank lines that don't follow a complete schematic block
               (loop '() all-raw-schematics)))

          ;; Non-blank line: Add to the current schematic block being read
          (else
           (loop (cons line current-schematic-lines) all-raw-schematics)))))))

;; Call the main function to execute the program
(main)
