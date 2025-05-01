
#lang racket

(struct cube (is-on x1 x2 y1 y2 z1 z2))

(define (parse-line line)
  (define parts (string-split line " "))
  (define command (car parts))
  (define ranges-str (cadr parts))
  (define coord-parts (string-split (string-replace (string-replace (string-replace (string-replace ranges-str "x=" "") "y=" "") "z=" "") ".." ",") ","))
  (define coords (map string->number coord-parts))
  (cube (equal? command "on")
        (list-ref coords 0)
        (list-ref coords 1)
        (list-ref coords 2)
        (list-ref coords 3)
        (list-ref coords 4)
        (list-ref coords 5)))

(define (parse-input input-str)
  (map parse-line (string-split input-str "\n")))

(define (get-intersection c1 c2)
  (define x1 (max (cube-x1 c1) (cube-x1 c2)))
  (define x2 (min (cube-x2 c1) (cube-x2 c2)))
  (define y1 (max (cube-y1 c1) (cube-y1 c2)))
  (define y2 (min (cube-y2 c1) (cube-y2 c2)))
  (define z1 (max (cube-z1 c1) (cube-z1 c2)))
  (define z2 (min (cube-z2 c1) (cube-z2 c2)))
  (if (or (> x1 x2) (> y1 y2) (> z1 z2))
      #f
      (let ([int-is-on (if (eqv? (cube-is-on c1) (cube-is-on c2))
                           (not (cube-is-on c1))
                           (cube-is-on c2))])
        (cube int-is-on x1 x2 y1 y2 z1 z2))))

(define (volume c)
  (define vol (* (- (cube-x2 c) (cube-x1 c) -1)
                 (- (cube-y2 c) (cube-y1 c) -1)
                 (- (cube-z2 c) (cube-z1 c) -1)))
  (if (cube-is-on c) vol (- vol)))

(define (process-cube current-cube final-list)
  (define intersections
    (filter identity
            (map (lambda (fc) (get-intersection fc current-cube)) final-list)))
  (define cubes-to-add
    (if (cube-is-on current-cube)
        (append intersections (list current-cube))
        intersections))
  (append cubes-to-add final-list))

(define (solve cubes)
  (define final-list (foldl process-cube '() cubes))
  (apply + (map volume final-list)))

(module+ main
  (require racket/cmdline)
  (define input-string (file->string "input.txt"))
  (define cubes (parse-input input-string))
  (define total-volume (solve cubes))
  (print total-volume))
