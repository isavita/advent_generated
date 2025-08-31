
(define (is-vowel? c)
  (or (char=? c #\a)
      (char=? c #\e)
      (char=? c #\i)
      (char=? c #\o)
      (char=? c #\u)))

(define (is-nice? str)
  (let ((len (string-length str)))
    (let loop ((idx 0)
               (vowels 0)
               (double-letter? #f))
      (cond
        ((= idx len)
         (and (>= vowels 3) double-letter?))
        (else
         (let* ((c1 (string-ref str idx))
                (new-vowels (if (is-vowel? c1) (+ vowels 1) vowels)))
           (if (< (+ idx 1) len)
               (let ((c2 (string-ref str (+ idx 1))))
                 (cond
                   ((and (char=? c1 #\a) (char=? c2 #\b)) #f)
                   ((and (char=? c1 #\c) (char=? c2 #\d)) #f)
                   ((and (char=? c1 #\p) (char=? c2 #\q)) #f)
                   ((and (char=? c1 #\x) (char=? c2 #\y)) #f)
                   (else
                    (let ((new-found-double-letter? (or double-letter? (char=? c1 c2))))
                      (loop (+ idx 1) new-vowels new-found-double-letter?)))))
               (loop (+ idx 1) new-vowels double-letter?))))))))

(define (main)
  (let ((nice-strings-count 0))
    (with-input-from-file "input.txt"
      (lambda ()
        (let loop ()
          (let ((line (read-line)))
            (if (eof-object? line)
                #f
                (begin
                  (when (is-nice? line)
                    (set! nice-strings-count (+ nice-strings-count 1)))
                  (loop)))))))
    (display nice-strings-count)
    (newline)))

(main)
