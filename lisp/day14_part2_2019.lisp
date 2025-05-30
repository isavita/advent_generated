
(defun split-string-by-str (string separator)
  (loop for i = 0 then (+ j (length separator))
        for j = (search separator string :start2 i :test 'string=)
        collect (subseq string i (or j (length string)))
        while j))

(defun parse-chemical (s)
  (let* ((parts (split-string-by-str s " "))
         (amount (parse-integer (first parts)))
         (name (second parts)))
    (list amount name)))

(defun calculate-ore (chem amount reactions ingredients surplus)
  (if (string= chem "ORE")
      amount
      (let* ((current-surplus (gethash chem surplus 0)))
        (if (>= current-surplus amount)
            (progn
              (setf (gethash chem surplus) (- current-surplus amount))
              0)
            (let* ((net-amount-needed (- amount current-surplus))
                   (reaction (gethash chem reactions))
                   (reaction-output-amount (first reaction))
                   (times (ceiling net-amount-needed reaction-output-amount))
                   (ore-produced 0))
              (setf (gethash chem surplus) 0)

              (loop for ingredient in (gethash chem ingredients) do
                (let* ((ingredient-amount (first ingredient))
                       (ingredient-name (second ingredient)))
                  (incf ore-produced (calculate-ore ingredient-name
                                                     (* ingredient-amount times)
                                                     reactions
                                                     ingredients
                                                     surplus))))

              (incf (gethash chem surplus 0) (- (* times reaction-output-amount) net-amount-needed))
              ore-produced)))))

(defun max-fuel (reactions ingredients ore-available)
  (let ((low 0)
        (high ore-available))
    (loop while (< low high) do
      (let* ((mid (ceiling (+ low high 1) 2))
             (current-surplus (make-hash-table :test 'equal))
             (ore-cost (calculate-ore "FUEL" mid reactions ingredients current-surplus)))
        (if (> ore-cost ore-available)
            (setf high (- mid 1))
            (setf low mid))))
    low))

(defun main ()
  (let ((reactions (make-hash-table :test 'equal))
        (ingredients (make-hash-table :test 'equal))
        (ore-available 1000000000000))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil)
            while line do
              (let* ((parts (split-string-by-str line " => "))
                     (output-str (second parts))
                     (inputs-str (first parts))
                     (output-chem (parse-chemical output-str))
                     (output-name (second output-chem))
                     (inputs-list (loop for s in (split-string-by-str inputs-str ", ")
                                        collect (parse-chemical s))))
                (setf (gethash output-name reactions) output-chem)
                (setf (gethash output-name ingredients) inputs-list))))
    (format t "~a~%" (max-fuel reactions ingredients ore-available))))

(main)
