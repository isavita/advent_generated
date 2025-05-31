
(defvar *position-cache* (make-hash-table :test 'equal))
(defvar *ok-cache* (make-hash-table :test 'equal))
(defvar *move-cache* (make-hash-table :test 'equal))
(defvar *solve-cache* (make-hash-table :test 'equal))

(defparameter *key-pad* (list "789" "456" "123" " 0A"))
(defparameter *robot-pad* (list " ^A" "<v>"))

(defparameter *key-pad-key* (apply #'concatenate 'string *key-pad*))
(defparameter *robot-pad-key* (apply #'concatenate 'string *robot-pad*))

(defun matrix-key (mat)
  (cond ((eq mat *key-pad*) *key-pad-key*)
        ((eq mat *robot-pad*) *robot-pad-key*)
        (t (apply #'concatenate 'string mat))))

(defun find-position (mat ch)
  (let* ((mat-key (matrix-key mat))
         (cache-key (list ch mat-key)))
    (multiple-value-bind (cached-pos found)
        (gethash cache-key *position-cache*)
      (when found
        (return-from find-position cached-pos)))

    (loop for i from 0 below (length mat) do
      (loop for j from 0 below (length (nth 0 mat)) do
        (when (char= (char (nth i mat) j) ch)
          (let ((pos (list i j)))
            (setf (gethash cache-key *position-cache*) pos)
            (return-from find-position pos)))))
    (list -1 -1)))

(defun ok (mat st seq)
  (let* ((mat-key (matrix-key mat))
         (cache-key (list (car st) (cadr st) seq mat-key)))
    (multiple-value-bind (cached-result found)
        (gethash cache-key *ok-cache*)
      (when found
        (return-from ok cached-result)))

    (let ((curr-i (car st))
          (curr-j (cadr st))
          (mat-rows (length mat))
          (mat-cols (length (nth 0 mat))))

      (loop for k from 0 below (length seq) do
        (when (char= (char (nth curr-i mat) curr-j) #\Space)
          (setf (gethash cache-key *ok-cache*) nil)
          (return-from ok nil))

        (let ((ch (char seq k)))
          (case ch
            (#\^ (decf curr-i))
            (#\v (incf curr-i))
            (#\< (decf curr-j))
            (#\> (incf curr-j))))

        (when (or (< curr-i 0) (>= curr-i mat-rows)
                  (< curr-j 0) (>= curr-j mat-cols))
          (setf (gethash cache-key *ok-cache*) nil)
          (return-from ok nil)))

      (setf (gethash cache-key *ok-cache*) t)
      t)))

(defun generate-moves (position objective pad)
  (let* ((pad-key (matrix-key pad))
         (cache-key (list (car position) (cadr position) objective pad-key)))
    (multiple-value-bind (cached-moves found)
        (gethash cache-key *move-cache*)
      (when found
        (return-from generate-moves cached-moves)))

    (let* ((obj-pos (find-position pad objective))
           (pos-i (car position))
           (pos-j (cadr position))
           (obj-i (car obj-pos))
           (obj-j (cadr obj-pos))
           (result ""))

      (setf result
            (concatenate 'string
                         (make-string (max 0 (- pos-j obj-j)) :initial-element #\<)
                         (make-string (max 0 (- pos-i obj-i)) :initial-element #\^)
                         (make-string (max 0 (- obj-i pos-i)) :initial-element #\v)
                         (make-string (max 0 (- obj-j pos-j)) :initial-element #\>)))

      (unless (ok pad position result)
        (setf result
              (concatenate 'string
                           (make-string (max 0 (- obj-j pos-j)) :initial-element #\>)
                           (make-string (max 0 (- pos-i obj-i)) :initial-element #\^)
                           (make-string (max 0 (- obj-i pos-i)) :initial-element #\v)
                           (make-string (max 0 (- pos-j obj-j)) :initial-element #\<))))

      (setf (gethash cache-key *move-cache*) result)
      result)))

(defun solve (code robots key-pad robot-pad max-robots)
  (let* ((cache-key (list code robots max-robots)))
    (multiple-value-bind (cached-result found)
        (gethash cache-key *solve-cache*)
      (when found
        (return-from solve cached-result)))

    (when (<= robots 0)
      (return-from solve (length code)))

    (let ((ret 0)
          (pos-i 3)
          (pos-j 2))
      (when (/= robots max-robots)
        (setf pos-i 0))

      (loop for ch across code do
        (let* ((current-pad (if (= robots max-robots) key-pad robot-pad))
               (moves (generate-moves (list pos-i pos-j) ch current-pad))
               (target-pos (find-position current-pad ch)))
          (setf pos-i (car target-pos))
          (setf pos-j (cadr target-pos))
          (incf ret (solve (concatenate 'string moves "A") (- robots 1) key-pad robot-pad max-robots))))

      (setf (gethash cache-key *solve-cache*) ret)
      ret)))

(defun main ()
  (let* ((max-robots 26)
         (total-ret 0))

    (with-open-file (infile "input.txt" :direction :input :if-does-not-exist :error)
      (loop for line = (read-line infile nil nil)
            while line do
              (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
                     (numeric-part 0))
                (unless (string= trimmed-line "")
                  (loop for ch across trimmed-line do
                    (when (char<= #\0 ch #\9)
                      (setf numeric-part (+ (* numeric-part 10) (- (char-code ch) (char-code #\0))))))

                  (let ((sv (solve trimmed-line max-robots *key-pad* *robot-pad* max-robots)))
                    (incf total-ret (* sv numeric-part)))))))
    (format t "~A~%" total-ret)))

(main)
