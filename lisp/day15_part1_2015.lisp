
(defstruct ingredient
  (name "")
  (capacity 0)
  (durability 0)
  (flavor 0)
  (texture 0))

(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun read-ingredients (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (let* ((parts (split-string line #\Space))
                         (capacity-str (string-trim (list #\,) (nth 2 parts)))
                         (durability-str (string-trim (list #\,) (nth 4 parts)))
                         (flavor-str (string-trim (list #\,) (nth 6 parts)))
                         (texture-str (string-trim (list #\,) (nth 8 parts))))
                    (make-ingredient :name (subseq (first parts) 0 (1- (length (first parts))))
                                     :capacity (parse-integer capacity-str)
                                     :durability (parse-integer durability-str)
                                     :flavor (parse-integer flavor-str)
                                     :texture (parse-integer texture-str))))))

(defun score (ingredients teaspoons)
  (let ((capacity 0) (durability 0) (flavor 0) (texture 0))
    (loop for i from 0 below (length ingredients)
          for ingredient = (nth i ingredients)
          for ts = (nth i teaspoons)
          do (incf capacity (* (ingredient-capacity ingredient) ts))
             (incf durability (* (ingredient-durability ingredient) ts))
             (incf flavor (* (ingredient-flavor ingredient) ts))
             (incf texture (* (ingredient-texture ingredient) ts)))
    (* (max 0 capacity)
       (max 0 durability)
       (max 0 flavor)
       (max 0 texture))))

(defun calculate-max-score (ingredients index remaining teaspoons)
  (if (= index (1- (length ingredients)))
      (score ingredients (append teaspoons (list remaining)))
      (loop for i from 0 to remaining
            maximize (calculate-max-score ingredients (1+ index) (- remaining i) (append teaspoons (list i))))))

(defun find-max-score (ingredients total-teaspoons)
  (calculate-max-score ingredients 0 total-teaspoons nil))

(defun main ()
  (let* ((ingredients (read-ingredients "input.txt"))
         (max-score (find-max-score ingredients 100)))
    (print max-score)))

(main)
