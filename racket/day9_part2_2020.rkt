
#lang racket

(module main racket/base
  (require racket/file
           racket/port
           racket/string
           racket/vector
           racket/base)

  (define invalid-num 14360655)

  (define (read-numbers filename)
    (with-input-from-file filename
      (lambda ()
        (for/vector ((line (in-lines)))
          (string->number line)))))

  (define (solve numbers target-sum)
    (let ((n (vector-length numbers)))
      (let loop-i ((i 0))
        (when (< i n)
          (let loop-j ((j (+ i 1)) (current-sum (vector-ref numbers i)))
            (cond
              ((= current-sum target-sum)
               (let ((contiguous-set (vector->list (vector-copy numbers i j))))
                 (let ((weakness (+ (apply min contiguous-set) (apply max contiguous-set))))
                   (print weakness)
                   (exit 0))))
              ((> current-sum target-sum)
               (loop-i (+ i 1)))
              (else
               (if (< j n)
                   (loop-j (+ j 1) (+ current-sum (vector-ref numbers j)))
                   (loop-i (+ i 1))))))))))

  (define numbers (read-numbers "input.txt"))
  (solve numbers invalid-num)
)
