
(defun main ()
  (let* ((input-value (with-open-file (f "input.txt")
                        (read f)))
         (scoreboard (make-array 2 :adjustable t :fill-pointer 2 :initial-contents '(3 7)))
         (elf1 0)
         (elf2 1))
    (loop while (< (fill-pointer scoreboard) (+ input-value 10))
          do (let* ((s1 (aref scoreboard elf1))
                    (s2 (aref scoreboard elf2))
                    (new-score (+ s1 s2)))
               (when (>= new-score 10)
                 (vector-push-extend (floor new-score 10) scoreboard))
               (vector-push-extend (mod new-score 10) scoreboard)
               (setf elf1 (mod (+ elf1 s1 1) (fill-pointer scoreboard)))
               (setf elf2 (mod (+ elf2 s2 1) (fill-pointer scoreboard)))))
    (loop for i from input-value to (+ input-value 9)
          do (princ (aref scoreboard i)))
    (terpri)))

(main)
