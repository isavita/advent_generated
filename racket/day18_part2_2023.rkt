
#lang racket
(require racket/string)
(require racket/file)

(struct coord (x y) #:transparent)

(define north (coord 0 -1))
(define west (coord -1 0))
(define south (coord 0 1))
(define east (coord 1 0))

(define (parse-input lines)
  (let ((vertices-reversed
         (foldl (lambda (line acc)
                  (let* ((current (car acc))
                         (parts (string-split line " "))
                         (color (list-ref parts 2))
                         (dir-char (string-ref color 7))
                         (len-str (substring color 2 7))
                         (len (string->number len-str 16))
                         (dir (case dir-char
                               ((#\3) north)
                               ((#\2) west)
                               ((#\1) south)
                               ((#\0) east)))
                         (next (coord (+ (coord-x current) (* (coord-x dir) len))
                                      (+ (coord-y current) (* (coord-y dir) len)))))
                    (cons next acc)))
                (list (coord 0 0))
                lines)))
    (reverse vertices-reversed)))

(define (shoelace vertices)
  (let ((wrapped-vertices (append (cdr vertices) (list (car vertices)))))
    (abs (quotient
          (for/sum ((v1 vertices) (v2 wrapped-vertices))
            (- (* (coord-x v1) (coord-y v2))
               (* (coord-y v1) (coord-x v2))))
          2))))

(define (perimeter vertices)
  (let ((wrapped-vertices (append (cdr vertices) (list (car vertices)))))
    (for/sum ((v1 vertices) (v2 wrapped-vertices))
      (+ (abs (- (coord-x v1) (coord-x v2)))
         (abs (- (coord-y v1) (coord-y v2)))))))

(define (calculate-polygon-area vertices)
  (+ (shoelace vertices)
     (quotient (perimeter vertices) 2)
     1))

(define (main)
  (define file-name "input.txt")
  (define lines (file->lines file-name))
  (define vertices (parse-input lines))
  (define result (calculate-polygon-area vertices))
  (print result))

(main)
