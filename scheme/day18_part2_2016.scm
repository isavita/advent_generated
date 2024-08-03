(define (read-first-row filename)
  (with-input-from-file filename
    (lambda ()
      (read-line))))

(define (is-trap left center right current-row)
  (let ((l (if (or (< left 0) (>= left (string-length current-row))) #\. (string-ref current-row left)))
        (c (string-ref current-row center))
        (r (if (or (< right 0) (>= right (string-length current-row))) #\. (string-ref current-row right))))
    (or (and (eq? l #\^) (eq? c #\^) (eq? r #\.))
        (and (eq? c #\^) (eq? r #\^) (eq? l #\.))
        (and (eq? l #\^) (eq? c #\.) (eq? r #\.))
        (and (eq? r #\^) (eq? c #\.) (eq? l #\.)))))

(define (count-char str char)
  (length (filter (lambda (c) (eq? c char)) (string->list str))))

(define (generate-next-row current-row)
  (let ((length (string-length current-row)))
    (apply string
           (map (lambda (j)
                  (if (is-trap (- j 1) j (+ j 1) current-row)
                      #\^
                      #\.))
                (iota length)))))

(define (count-safe-tiles first-row total-rows)
  (define safe-count (count-char first-row #\.))
  (define current-row first-row)
  
  (do ((i 1 (+ i 1)))
      ((= i total-rows) safe-count)
    (set! current-row (generate-next-row current-row))
    (set! safe-count (+ safe-count (count-char current-row #\.)))))

(define (iota n)
  (if (= n 0)
      '()
      (cons (- n 1) (iota (- n 1)))))

(define total-rows 400000)
(define first-row (read-first-row "input.txt"))
(display (count-safe-tiles first-row total-rows))