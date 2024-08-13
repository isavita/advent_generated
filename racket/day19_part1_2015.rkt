#lang racket

(define (main)
  (define input (file->lines "input.txt"))
  (define replacements '())
  (define molecule "")
  
  (for ([line input])
    (cond
      [(string? (regexp-match #px"^\\s*$" line)) (void)]
      [(string-contains? line " => ")
       (set! replacements (cons line replacements))]
      [else (set! molecule line)]))
  
  (define molecules (make-hash))
  
  (for ([replacement replacements])
    (define parts (string-split replacement " => "))
    (define from (first parts))
    (define to (second parts))
    
    (for ([i (in-range (string-length molecule))])
      (when (string-prefix? (substring molecule i) from)
        (define new-molecule (string-append (substring molecule 0 i) to (substring molecule (+ i (string-length from)) (string-length molecule))))
        (hash-set! molecules new-molecule #t))))
  
  (displayln (hash-count molecules)))

(main)