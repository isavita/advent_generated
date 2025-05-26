
(defun read-file-into-stripped-string (filepath)
  (with-open-file (s filepath :direction :input :if-does-not-exist :error)
    (let* ((len (file-length s))
           (data (make-string len)))
      (read-sequence data s)
      (string-trim '(#\Newline #\Return #\Space #\Tab) data))))

(defun calculate-score (stream)
  (loop with total-score = 0
        with current-score = 0
        with in-garbage = nil
        with ignore-next = nil
        for char across stream
        do (cond
             (ignore-next (setq ignore-next nil))
             ((char= char #\!) (setq ignore-next t))
             ((and (char= char #\<) (not in-garbage)) (setq in-garbage t))
             ((and (char= char #\>) in-garbage) (setq in-garbage nil))
             ((and (char= char #\{) (not in-garbage)) (incf current-score))
             ((and (char= char #\}) (not in-garbage))
              (incf total-score current-score)
              (decf current-score)))
        finally (return total-score)))

(defun main ()
  (let ((stream (read-file-into-stripped-string "input.txt")))
    (princ (calculate-score stream))))

(main)
