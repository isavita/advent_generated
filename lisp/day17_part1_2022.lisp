
(defun get-rock-shapes ()
  (list
   (list '(0 0) '(1 0) '(2 0) '(3 0))
   (list '(1 0) '(0 1) '(1 1) '(2 1) '(1 2))
   (list '(0 0) '(1 0) '(2 0) '(2 1) '(2 2))
   (list '(0 0) '(0 1) '(0 2) '(0 3))
   (list '(0 0) '(1 0) '(0 1) '(1 1))))

(defun can-move (rock direction chamber)
  (loop for (x y) in rock
        for new-x = (cond ((eq direction :left) (1- x))
                          ((eq direction :right) (1+ x))
                          (t x))
        for new-y = (cond ((eq direction :down) (1- y))
                          (t y))
        do
        (when (or (< new-x 0) (> new-x 6) (< new-y 1))
          (return-from can-move nil))
        (when (gethash (list new-x new-y) chamber)
          (return-from can-move nil))
        collect (list new-x new-y)))

(defun simulate (jet-pattern total-rocks)
  (let* ((rock-shapes (get-rock-shapes))
         (chamber (make-hash-table :test 'equalp))
         (highest-y 0)
         (jet-len (length jet-pattern))
         (jet-index 0))
    (loop for x from 0 to 6 do
      (setf (gethash (list x 0) chamber) t))
    (loop for rock-number from 0 below total-rocks do
      (let* ((shape (nth (mod rock-number (length rock-shapes)) rock-shapes))
             (current-rock-x 2)
             (current-rock-y (+ highest-y 4))
             (rock (loop for (dx dy) in shape collect (list (+ current-rock-x dx) (+ current-rock-y dy)))))
        (loop
          (let ((jet-dir (char jet-pattern (mod jet-index jet-len)))
                (moved-rock nil))
            (incf jet-index)
            (setf moved-rock
                  (cond
                    ((char= jet-dir #\>) (can-move rock :right chamber))
                    ((char= jet-dir #\<) (can-move rock :left chamber))
                    (t (error "Invalid jet direction"))))
            (when moved-rock
              (setf rock moved-rock))
            (let ((moved-down (can-move rock :down chamber)))
              (if moved-down
                  (setf rock moved-down)
                  (progn
                    (loop for (x y) in rock do
                      (setf (gethash (list x y) chamber) t)
                      (when (> y highest-y)
                        (setf highest-y y)))
                    (return))))))))
    highest-y))

(defun read-input (filename)
  (with-open-file (s filename :direction :input)
    (read-line s nil nil)))

(defun main ()
  (let* ((jet-pattern (read-input "input.txt"))
         (total-rocks 2022)
         (final-height (simulate jet-pattern total-rocks)))
    (print final-height)))

(main)
