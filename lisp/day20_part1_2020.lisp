
(defun string-reverse (s)
  (coerce (reverse (coerce s 'list)) 'string))

(defun get-borders (tile)
  (let* ((size (length tile))
         (top (car tile))
         (bottom (nth (1- size) tile))
         (left (loop for row in tile collect (char row 0)))
         (right (loop for row in tile collect (char row (1- size)))))
    (list top
          bottom
          (coerce left 'string)
          (coerce right 'string))))

(defun parse-input (filename)
  (let ((tiles (make-hash-table :test 'eql))
        (current-tile-id nil)
        (current-tile-data nil))
    (with-open-file (in filename :direction :input)
      (loop for line = (read-line in nil nil)
            while line do
            (cond ((search "Tile" line)
                   (when current-tile-id
                     (setf (gethash current-tile-id tiles) current-tile-data))
                   (setf current-tile-id (parse-integer (subseq line 5 (1- (length line)))))
                   (setf current-tile-data nil))
                  ((string/= line "")
                   (push line current-tile-data)))))
    (when current-tile-id
      (setf (gethash current-tile-id tiles) (nreverse current-tile-data)))
    tiles))

(defun main ()
  (let* ((tiles (parse-input "input.txt"))
         (borders-map (make-hash-table :test 'equal))
         (corner-tiles nil))
    (maphash #'(lambda (tile-id tile-data)
                 (loop for border in (get-borders tile-data) do
                       (push tile-id (gethash border borders-map nil))
                       (push tile-id (gethash (string-reverse border) borders-map nil))))
             tiles)
    (maphash #'(lambda (tile-id tile-data)
                 (let ((matching-borders 0))
                   (loop for border in (get-borders tile-data) do
                         (when (= (length (gethash border borders-map)) 1)
                           (incf matching-borders)))
                   (when (= matching-borders 2)
                     (push tile-id corner-tiles))))
             tiles)
    (format t "~a~%" (apply #'* corner-tiles))))

(main)
