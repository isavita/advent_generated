
#lang racket

(require racket/match
         racket/string)

;; --- Constants ---
(define WIDTH 50)
(define HEIGHT 6)

;; --- Screen Representation ---
;; A mutable vector of mutable vectors, representing rows.
;; #t means pixel is lit, #f means off.
(define (make-screen width height)
  (build-vector height (lambda (_) (make-vector width #f))))

;; --- Parsing Instructions ---
(define (parse-instruction line)
  (cond
    [(regexp-match #px"rect (\\d+)x(\\d+)" line)
     => (lambda (m) (list 'rect (string->number (cadr m)) (string->number (caddr m))))]
    [(regexp-match #px"rotate row y=(\\d+) by (\\d+)" line)
     => (lambda (m) (list 'rotate-row (string->number (cadr m)) (string->number (caddr m))))]
    [(regexp-match #px"rotate column x=(\\d+) by (\\d+)" line)
     => (lambda (m) (list 'rotate-col (string->number (cadr m)) (string->number (caddr m))))]
    [else (error 'parse-instruction "Invalid instruction format: ~a" line)]))

;; --- Screen Operations (Mutating) ---

;; rect AxB: Turns on pixels in a rectangle A wide and B tall at the top-left.
(define (apply-rect! screen width height)
  (for* ([r (in-range height)]
         [c (in-range width)])
    (vector-set! (vector-ref screen r) c #t)))

;; rotate row y=A by B: Shifts row A right by B pixels, wrapping around.
(define (apply-rotate-row! screen row-index amount)
  (define row (vector-ref screen row-index))
  (define len (vector-length row))
  (define shift (modulo amount len))
  (when (> shift 0) ; No need to rotate if shift is 0
    (define temp-row (vector-copy row)) ; Copy original row state
    (for ([c (in-range len)])
      (vector-set! row c (vector-ref temp-row (modulo (- c shift) len))))))

;; rotate column x=A by B: Shifts column A down by B pixels, wrapping around.
(define (apply-rotate-col! screen col-index amount)
  (define len HEIGHT)
  (define shift (modulo amount len))
  (when (> shift 0) ; No need to rotate if shift is 0
    ;; Store original column values temporarily
    (define temp-col (build-vector len (lambda (r) (vector-ref (vector-ref screen r) col-index))))
    ;; Update the screen column using values from the temp vector
    (for ([r (in-range len)])
      (vector-set! (vector-ref screen r)
                   col-index
                   (vector-ref temp-col (modulo (- r shift) len))))))

;; Dispatches to the correct operation based on the parsed instruction
(define (apply-instruction! screen instruction)
  (match instruction
    [(list 'rect w h)         (apply-rect! screen w h)]
    [(list 'rotate-row r amt) (apply-rotate-row! screen r amt)]
    [(list 'rotate-col c amt) (apply-rotate-col! screen c amt)]
    [_ (error 'apply-instruction! "Unknown instruction type: ~a" instruction)]))

;; --- Calculation and Display ---

;; Counts the number of lit pixels (#t) on the screen.
(define (count-lit-pixels screen)
  (for*/sum ([row (in-vector screen)]
             [pixel (in-vector row)])
    (if pixel 1 0)))

;; Displays the screen state to standard output.
(define (display-screen screen)
  (for ([row (in-vector screen)])
    (for ([pixel (in-vector row)])
      (display (if pixel "#" ".")))
    (newline)))

;; --- Main Execution ---
(define (main)
  (define filename "input.txt")
  (unless (file-exists? filename)
    (eprintf "Error: Input file '~a' not found.\n" filename)
    (exit 1))

  (define lines (file->lines filename))
  (define instructions (map parse-instruction lines))

  ;; Initialize the screen
  (define screen (make-screen WIDTH HEIGHT))

  ;; Process all instructions by mutating the screen
  (for ([instr instructions])
    (apply-instruction! screen instr))

  ;; Part 1: Count lit pixels
  (printf "Part 1: ~a\n" (count-lit-pixels screen))

  ;; Part 2: Display the final screen state
  (printf "Part 2:\n")
  (display-screen screen))

;; Run the main function
(main)
