
(defun main ()
  (let* ((steps (with-open-file (f "input.txt") (read f)))
         (buffer (list 0))
         (current-pos 0))
    (loop for i from 1 below 2018
          do (let* ((buf-len (length buffer))
                    (insert-pos (1+ (mod (+ current-pos steps) buf-len))))
               (setq buffer (append (subseq buffer 0 insert-pos)
                                    (list i)
                                    (nthcdr insert-pos buffer)))
               (setq current-pos insert-pos)))
    (let* ((final-buf-len (length buffer))
           (result-pos (mod (1+ current-pos) final-buf-len)))
      (format t "~a~%" (nth result-pos buffer)))))

(main)
