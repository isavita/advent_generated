(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (read-line))))

(define (calculate-sum captcha step)
  (let* ((length (string-length captcha))
         (half-step (/ length 2)))
    (define (helper idx acc)
      (if (= idx length)
          acc
          (let* ((next-idx (modulo (+ idx step) length))
                 (current (string-ref captcha idx))
                 (next (string-ref captcha next-idx)))
            (helper (+ idx 1) 
                    (if (char=? current next) 
                        (+ acc (string->number (string current))) 
                        acc)))))
    (helper 0 0)))

(define (main)
  (let* ((captcha (read-input "input.txt"))
         (part1-sum (calculate-sum captcha 1))
         (part2-sum (calculate-sum captcha (/ (string-length captcha) 2))))
    (display (string-append "Part 1: " (number->string part1-sum) "\n"))
    (display (string-append "Part 2: " (number->string part2-sum) "\n"))))

(main)