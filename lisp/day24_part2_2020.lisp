
(defparameter *directions*
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "e" h) '(1 0))
    (setf (gethash "se" h) '(0 1))
    (setf (gethash "sw" h) '(-1 1))
    (setf (gethash "w" h) '(-1 0))
    (setf (gethash "nw" h) '(0 -1))
    (setf (gethash "ne" h) '(1 -1))
    h))

(defun flip-tiles (instructions)
  (let ((black-tiles (make-hash-table :test 'equal)))
    (dolist (instruction instructions black-tiles)
      (let ((x 0)
            (y 0)
            (i 0))
        (loop while (< i (length instruction))
              do (let* ((char (char instruction i))
                        (dir-key nil))
                   (cond
                     ((or (char= char #\s) (char= char #\n))
                      (setf dir-key (subseq instruction i (+ i 2)))
                      (incf i 2))
                     (t
                      (setf dir-key (subseq instruction i (1+ i)))
                      (incf i 1)))
                   (let ((delta (gethash dir-key *directions*)))
                     (incf x (first delta))
                     (incf y (second delta)))))
        (let ((coord (list x y)))
          (if (gethash coord black-tiles)
              (remhash coord black-tiles)
              (setf (gethash coord black-tiles) t)))))))

(defun count-adjacent-black-tiles (tile black-tiles)
  (let ((tx (first tile))
        (ty (second tile)))
    (loop for dir-delta being the hash-value of *directions*
          sum (let* ((nx (+ tx (first dir-delta)))
                     (ny (+ ty (second dir-delta)))
                     (neighbor-coord (list nx ny)))
                (if (gethash neighbor-coord black-tiles) 1 0)))))

(defun simulate-days (initial-black-tiles days)
  (let ((black-tiles initial-black-tiles))
    (loop for day from 0 below days
          do (let ((new-black-tiles (make-hash-table :test 'equal))
                   (candidates (make-hash-table :test 'equal)))
               (loop for tile-coord being the hash-key of black-tiles
                     do (setf (gethash tile-coord candidates) t))
               (loop for tile-coord being the hash-key of black-tiles
                     do (let* ((tx (first tile-coord))
                                (ty (second tile-coord)))
                          (loop for dir-delta being the hash-value of *directions*
                                do (let* ((nx (+ tx (first dir-delta)))
                                          (ny (+ ty (second dir-delta)))
                                          (neighbor-coord (list nx ny)))
                                     (setf (gethash neighbor-coord candidates) t)))))
               (loop for tile-coord being the hash-key of candidates
                     do (let* ((is-black (gethash tile-coord black-tiles))
                                (adjacent-black-count (count-adjacent-black-tiles tile-coord black-tiles)))
                          (cond
                            ((and is-black (or (= adjacent-black-count 0) (> adjacent-black-count 2)))
                             nil)
                            ((and (not is-black) (= adjacent-black-count 2))
                             (setf (gethash tile-coord new-black-tiles) t))
                            (is-black
                             (setf (gethash tile-coord new-black-tiles) t)))))
               (setf black-tiles new-black-tiles)))
    (hash-table-count black-tiles)))

(defun main ()
  (let* ((input-lines (with-open-file (stream "input.txt")
                        (loop for line = (read-line stream nil :eof)
                              until (eq line :eof)
                              for trimmed-line = (string-trim '(#\Space #\Tab #\Newline #\Return) line)
                              when (not (string= "" trimmed-line))
                              collect trimmed-line)))
         (initial-black-tiles (flip-tiles input-lines))
         (result (simulate-days initial-black-tiles 100)))
    (princ result)
    (terpri)))

(main)
