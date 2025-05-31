
(defstruct claim
  id x y width height)

(defun parse-claim-line (line)
  (let* ((at-pos (position #\@ line))
         (id-str (subseq line 1 (1- at-pos)))
         (id (parse-integer id-str))

         (colon-pos (position #\: line))
         (coords-str (subseq line (+ at-pos 2) colon-pos))
         (comma-pos (position #\, coords-str))
         (x (parse-integer (subseq coords-str 0 comma-pos)))
         (y (parse-integer (subseq coords-str (1+ comma-pos))))

         (dims-str (subseq line (+ colon-pos 2)))
         (x-char-pos (position #\x dims-str))
         (width (parse-integer (subseq dims-str 0 x-char-pos)))
         (height (parse-integer (subseq dims-str (1+ x-char-pos)))))
    (make-claim :id id :x x :y y :width width :height height)))

(defun read-claims (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          collect (parse-claim-line line))))

(defun main ()
  (let* ((claims (read-claims "input.txt"))
         (fabric-size 1000)
         (fabric (make-array (list fabric-size fabric-size) :initial-element 0)))

    (loop for claim in claims do
      (loop for y from (claim-y claim) below (+ (claim-y claim) (claim-height claim)) do
        (loop for x from (claim-x claim) below (+ (claim-x claim) (claim-width claim)) do
          (incf (aref fabric y x)))))

    (loop for claim in claims do
      (let ((overlap-found nil))
        (loop for y from (claim-y claim) below (+ (claim-y claim) (claim-height claim)) do
          (loop for x from (claim-x claim) below (+ (claim-x claim) (claim-width claim)) do
            (when (> (aref fabric y x) 1)
              (setf overlap-found t)
              (return)))
          (when overlap-found (return)))
        (unless overlap-found
          (format t "~a~%" (claim-id claim))
          (return))))))

(main)
