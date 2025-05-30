
(defparameter *shape-scores*
  '((#\X . 1)
    (#\Y . 2)
    (#\Z . 3)))

(defparameter *outcome-scores*
  '((#\X . 0)
    (#\Y . 3)
    (#\Z . 6)))

(defparameter *opponent-to-player-shape*
  '((#\A . #\X)
    (#\B . #\Y)
    (#\C . #\Z)))

(defparameter *lose-shape-map*
  '((#\A . #\Z)
    (#\B . #\X)
    (#\C . #\Y)))

(defparameter *win-shape-map*
  '((#\A . #\Y)
    (#\B . #\Z)
    (#\C . #\X)))

(defun get-value (key alist)
  (cdr (assoc key alist)))

(defun determine-player-shape (opponent-char outcome-char)
  (cond
    ((char= outcome-char #\Y) (get-value opponent-char *opponent-to-player-shape*))
    ((char= outcome-char #\X) (get-value opponent-char *lose-shape-map*))
    ((char= outcome-char #\Z) (get-value opponent-char *win-shape-map*))))

(defun calculate-round-score (opponent-char outcome-char)
  (let* ((player-shape (determine-player-shape opponent-char outcome-char))
         (shape-score (get-value player-shape *shape-scores*))
         (outcome-score (get-value outcome-char *outcome-scores*)))
    (+ shape-score outcome-score)))

(defun main ()
  (let ((total-score 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (let ((opponent-char (char line 0))
                     (outcome-char (char line 2)))
                 (incf total-score (calculate-round-score opponent-char outcome-char)))))
    (print total-score)))

(main)
