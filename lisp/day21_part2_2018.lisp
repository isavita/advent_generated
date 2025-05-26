
(defun main ()
  (let* ((r5 0)
         (seen (make-hash-table :test 'eql))
         (last-unique 0))
    (loop
      (let ((r3 (logior r5 65536)))
        (setf r5 7586220)
        (loop
          (let ((r1 (logand r3 255)))
            (setf r5 (mod (* (mod (+ r5 r1) 16777216) 65899) 16777216))
            (if (< r3 256)
                (if (gethash r5 seen)
                    (progn (princ "Part Two Answer: ") (princ last-unique) (terpri) (return-from main nil))
                    (progn
                      (setf (gethash r5 seen) t)
                      (setf last-unique r5)
                      (return)))
                (setf r3 (floor r3 256)))))))))

(main)
