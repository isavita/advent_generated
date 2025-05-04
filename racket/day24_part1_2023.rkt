
#lang racket

(struct coord (x y z) #:transparent)
(struct point (pos vel) #:transparent)

(define (parse-line line)
  (define parts (string-split line " @ "))
  (define pos-parts (string-split (car parts) ", "))
  (define vel-parts (string-split (cadr parts) ", "))
  (define pos-nums (map string->number pos-parts))
  (define vel-nums (map string->number vel-parts))
  (point (coord (list-ref pos-nums 0) (list-ref pos-nums 1) (list-ref pos-nums 2))
         (coord (list-ref vel-nums 0) (list-ref vel-nums 1) (list-ref vel-nums 2))))

(define (parse-input filename)
  (with-input-from-file filename
    (lambda () (map parse-line (port->lines)))))

(define (is-intersecting-2d p1 p2)
  (define p1-pos (point-pos p1))
  (define p1-vel (point-vel p1))
  (define p2-pos (point-pos p2))
  (define p2-vel (point-vel p2))
  (define det (- (* (coord-x p1-vel) (coord-y p2-vel))
                 (* (coord-x p2-vel) (coord-y p1-vel))))
  (if (zero? det)
      (values #f (coord 0 0 0) 0 0)
      (let* ([t1-num (- (* (coord-y p2-vel) (- (coord-x p2-pos) (coord-x p1-pos)))
                       (* (coord-x p2-vel) (- (coord-y p2-pos) (coord-y p1-pos))))]
             [t2-num (- (* (coord-y p1-vel) (- (coord-x p2-pos) (coord-x p1-pos)))
                       (* (coord-x p1-vel) (- (coord-y p2-pos) (coord-y p1-pos))))]
             [t1 (/ t1-num det)]
             [t2 (/ t2-num det)]
             [intersect-x (+ (coord-x p1-pos) (* (coord-x p1-vel) t1))]
             [intersect-y (+ (coord-y p1-pos) (* (coord-y p1-vel) t1))]
             [intersect-coord (coord intersect-x intersect-y 0)])
        (values #t intersect-coord t1 t2))))

(define (solve filename min-val max-val)
  (define points (parse-input filename))
  (define n (length points))
  (define count 0)
  (for ([i (in-range n)])
    (for ([j (in-range i)])
      (define p1 (list-ref points i))
      (define p2 (list-ref points j))
      (define-values (is-intersecting? intersect-coord t1 t2) (is-intersecting-2d p1 p2))
      (when is-intersecting?
        (define intersect-x (coord-x intersect-coord))
        (define intersect-y (coord-y intersect-coord))
        (define is-in-bound? (and (>= intersect-x min-val) (<= intersect-x max-val)
                                  (>= intersect-y min-val) (<= intersect-y max-val)))
        (define is-future? (and (>= t1 0) (>= t2 0)))
        (when (and is-in-bound? is-future?)
          (set! count (add1 count))))))
  count)

(define (main)
  (define min-val 200000000000000)
  (define max-val 400000000000000)
  (define result (solve "input.txt" min-val max-val))
  (printf "~a~n" result))

(main)
