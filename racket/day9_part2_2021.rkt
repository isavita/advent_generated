
#lang racket

(require racket/string)
(require racket/list)
(require racket/hash)
(require racket/port)

(define (read-heightmap filename)
  (with-input-from-file filename
    (lambda ()
      (map (lambda (line)
             (map (lambda (char)
                    (- (char->integer char) (char->integer #\0)))
                  (string->list (string-trim line))))
           (port->lines (current-input-port))))))

(define (get-height heightmap x y)
  (let* ([rows (length heightmap)]
         [cols (if (null? heightmap) 0 (length (car heightmap)))])
    (if (and (>= x 0) (< x cols) (>= y 0) (< y rows))
        (list-ref (list-ref heightmap y) x)
        #f)))

(define (is-low-point heightmap x y)
  (let ([h (get-height heightmap x y)])
    (and h
         (let ([neighbors (list (get-height heightmap x (- y 1))
                                (get-height heightmap x (+ y 1))
                                (get-height heightmap (- x 1) y)
                                (get-height heightmap (+ x 1) y))])
           (andmap (lambda (nh) (or (not nh) (> nh h))) neighbors)))))

(define (explore-basin heightmap x y visited)
  (let* ([h (get-height heightmap x y)]
         [pos (cons x y)])
    (if (or (not h)
            (= h 9)
            (hash-has-key? visited pos))
        0
        (begin
          (hash-set! visited pos #t)
          (+ 1
             (explore-basin heightmap x (- y 1) visited)
             (explore-basin heightmap (- x 1) y visited)
             (explore-basin heightmap x (+ y 1) visited)
             (explore-basin heightmap (+ x 1) y visited))))))

(define (main)
  (let* ([heightmap (read-heightmap "input.txt")]
         [rows (length heightmap)]
         [cols (if (null? heightmap) 0 (length (car heightmap)))]
         [basin-sizes '()]
         [visited (make-hash)])
    (for* ([y (in-range rows)]
           [x (in-range cols)])
      (when (is-low-point heightmap x y)
        (let ([size (explore-basin heightmap x y visited)])
          (set! basin-sizes (cons size basin-sizes)))))
    (let ([sorted-sizes (sort basin-sizes >)])
      (if (>= (length sorted-sizes) 3)
          (print (* (car sorted-sizes) (cadr sorted-sizes) (caddr sorted-sizes)))
          (error "Less than 3 basins found")))))

(module+ main
  (main))
