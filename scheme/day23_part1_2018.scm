
(define (manhattan-distance p1 p2)
  (let ((x1 (car p1)) (y1 (cadr p1)) (z1 (caddr p1))
        (x2 (car p2)) (y2 (cadr p2)) (z2 (caddr p2)))
    (+ (abs (- x1 x2)) (abs (- y1 y2)) (abs (- z1 z2)))))

(define (parse-nanobot line)
  (let ((parts (string-split line ", ")))
    (let* ((pos-str (cadr (string-split (car parts) "=")))
           (r-str (cadr (string-split (cadr parts) "=")))
           (pos-parts (map string->number (string-split pos-str ","))))
      (list pos-parts (string->number r-str)))))

(define (in-range? nanobot1 nanobot2)
  (let ((pos1 (car nanobot1)) (r1 (cadr nanobot1))
        (pos2 (car nanobot2)))
    (<= (manhattan-distance pos1 pos2) r1)))

(define (solve filename)
  (let* ((lines (with-input-from-file filename read-lines))
         (nanobots (map parse-nanobot lines))
         (strongest-nanobot (apply max (lambda (x y) (if (> (cadr x) (cadr y)) x y)) nanobots))
         (in-range-count (length (filter (lambda (nanobot) (in-range? strongest-nanobot nanobot)) nanobots))))
    in-range-count))

(display (solve "input.txt"))
(newline)
