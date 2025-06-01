
(defstruct file-segment
  id
  start
  end)

(defun main ()
  (with-open-file (in "input.txt" :direction :input)
    (let* ((line (read-line in))
           (disk-size 0)
           (disk nil)
           (files nil))

      ;; Calculate disk size and initialize disk
      (loop for char across line do
        (incf disk-size (digit-char-p char)))
      (setf disk (make-array disk-size :initial-element nil))

      ;; Create initial disk layout
      (let ((file-id 0)
            (current-pos 0)
            (is-file t))
        (loop for char across line do
          (let ((length (digit-char-p char)))
            (loop for i from 0 below length do
              (setf (aref disk current-pos) (if is-file file-id nil))
              (incf current-pos))
            (when is-file
              (incf file-id))
            (setf is-file (not is-file)))))

      ;; Build file segments list
      (let ((curr-id nil)
            (start-idx 0))
        (loop for i from 0 below disk-size
              for val = (aref disk i)
              do
          (cond ((null val) ; Free space
                 (setf curr-id nil))
                (t ; File block
                 (unless (eql val curr-id)
                   (setf curr-id val)
                   (setf start-idx i))
                 (when (or (= i (1- disk-size))
                           (not (eql (aref disk (1+ i)) val)))
                   (push (make-file-segment :id curr-id :start start-idx :end i) files))))))

      ;; Process files from highest ID to lowest
      (setf files (sort files #'> :key #'file-segment-id)) ; Sort by ID descending

      (loop for file in files do
        (let* ((file-len (1+ (- (file-segment-end file) (file-segment-start file))))
               (leftmost-span -1)
               (span-len 0))

          ;; Find leftmost suitable free space
          (loop for i from 0 below (file-segment-start file) do
            (if (null (aref disk i))
                (progn
                  (when (= span-len 0)
                    (setf leftmost-span i))
                  (incf span-len)
                  (when (= span-len file-len)
                    (return)))
                (progn
                  (setf span-len 0)
                  (setf leftmost-span -1))))

          ;; Move file if we found suitable space
          (when (and (/= leftmost-span -1) (= span-len file-len))
            ;; Clear old location
            (loop for i from (file-segment-start file) to (file-segment-end file) do
              (setf (aref disk i) nil))
            ;; Copy file to new location
            (loop for i from 0 below file-len do
              (setf (aref disk (+ leftmost-span i)) (file-segment-id file))))))

      ;; Calculate checksum
      (let ((checksum 0))
        (loop for i from 0 below disk-size
              for val = (aref disk i)
              do
          (unless (null val)
            (incf checksum (* i val))))
        (print checksum)))))

(main)
