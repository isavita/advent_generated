(define (spinlock step-count total-inserts)
  (let loop ((current-pos 0) (value-after-zero -1) (current-insert 1))
    (if (> current-insert total-inserts)
        value-after-zero
        (let* ((new-pos (modulo (+ current-pos step-count) current-insert)))
          (loop (+ new-pos 1)
                (if (= new-pos 0) current-insert value-after-zero)
                (+ current-insert 1))))))

(define (main)
  (let ((step-count (with-input-from-file "input.txt"
                      (lambda () (read)))))
    (display (spinlock step-count 50000000))))

(main)