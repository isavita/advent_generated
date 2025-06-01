
(defvar *happiness-map* (make-hash-table :test 'equal))
(defvar *all-guests* (make-hash-table :test 'equal))

(defun simple-split (string delimiter)
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun parse-line (line)
  (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
         (parts (simple-split trimmed-line #\Space))
         (person1 (first parts))
         (gain-or-lose (nth 2 parts))
         (change-str (nth 3 parts))
         (person2 (subseq (car (last parts)) 0 (- (length (car (last parts))) 1)))
         (change (parse-integer change-str)))
    (setf (gethash person1 *all-guests*) t)
    (setf (gethash person2 *all-guests*) t)
    (setf (gethash (list person1 person2) *happiness-map*)
          (if (string= gain-or-lose "gain") change (- change)))))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          do (parse-line line)))
  (setf (gethash "You" *all-guests*) t))

(defun get-happiness-value (p1 p2)
  (gethash (list p1 p2) *happiness-map* 0))

(defun calculate-arrangement-happiness (arrangement)
  (loop for i from 0 below (length arrangement)
        sum (let* ((p1 (nth i arrangement))
                   (p2 (nth (mod (1+ i) (length arrangement)) arrangement)))
              (+ (get-happiness-value p1 p2)
                 (get-happiness-value p2 p1)))))

(defun permutations (lst)
  (cond ((null lst) (list nil))
        (t (loop for x in lst
                 append (loop for p in (permutations (remove x lst :count 1 :test 'string=))
                              collect (cons x p))))))

(defun main ()
  (read-input "input.txt")
  (let* ((guest-list (loop for guest being the hash-key of *all-guests* collect guest))
         (max-happiness 0))
    (dolist (arrangement (permutations guest-list))
      (setf max-happiness (max max-happiness (calculate-arrangement-happiness arrangement))))
    (print max-happiness)))

(main)
