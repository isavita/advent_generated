
(defun split-string (s &optional (delimiter " "))
  (loop for i = 0 then (1+ j)
        as j = (search delimiter s :start2 i)
        collect (subseq s i (or j (length s)))
        while j))

(defstruct node
  value
  prev
  next)

(defun insert-after (current-node value)
  (let* ((new-node (make-node :value value))
         (next-node (node-next current-node)))
    (setf (node-next new-node) next-node)
    (setf (node-prev new-node) current-node)
    (setf (node-next current-node) new-node)
    (setf (node-prev next-node) new-node)
    new-node))

(defun remove-node (node-to-remove)
  (let* ((prev (node-prev node-to-remove))
         (next (node-next node-to-remove)))
    (setf (node-next prev) next)
    (setf (node-prev next) prev)
    (node-value node-to-remove)))

(defun main ()
  (let ((players 0)
        (last-marble 0)
        scores
        current-marble-node
        (current-player 0))

    (with-open-file (f "input.txt")
      (let* ((line (read-line f))
             (parts (split-string line)))
        (setf players (parse-integer (nth 0 parts)))
        (setf last-marble (* 100 (parse-integer (nth 6 parts))))))

    (setf scores (make-array players :initial-element 0))

    (setf current-marble-node (make-node :value 0))
    (setf (node-prev current-marble-node) current-marble-node)
    (setf (node-next current-marble-node) current-marble-node)

    (loop for marble from 1 to last-marble do
      (if (= (mod marble 23) 0)
          (let ((removed-marble-node current-marble-node))
            (loop for i from 1 to 7 do
              (setf removed-marble-node (node-prev removed-marble-node)))
            (incf (aref scores current-player) marble)
            (incf (aref scores current-player) (remove-node removed-marble-node))
            (setf current-marble-node (node-next removed-marble-node)))
          (progn
            (setf current-marble-node (node-next current-marble-node))
            (setf current-marble-node (insert-after current-marble-node marble))))
      (setf current-player (mod (1+ current-player) players)))

    (print (loop for score across scores maximize score))))

(main)
