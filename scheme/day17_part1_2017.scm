(define (spinlock step)
  (let loop ((buffer '(0))
             (current 0)
             (n 1))
    (if (> n 2017)
        (let ((index (modulo (+ 1 current) (length buffer))))
          (list-ref buffer index))
        (let* ((new-position (modulo (+ current step) n))
               (new-buffer (append (take buffer (+ 1 new-position))
                                   (list n)
                                   (drop buffer (+ 1 new-position)))))
          (loop new-buffer (+ new-position 1) (+ n 1))))))

(define (main)
  (let ((step (with-input-from-file "input.txt"
                (lambda () (read)))))
    (display (spinlock step))))

(main)