
(defconstant +weapons+
  '(("Dagger" 8 4 0)
    ("Shortsword" 10 5 0)
    ("Warhammer" 25 6 0)
    ("Longsword" 40 7 0)
    ("Greataxe" 74 8 0)))

(defconstant +armor+
  '(("None" 0 0 0)
    ("Leather" 13 0 1)
    ("Chainmail" 31 0 2)
    ("Splintmail" 53 0 3)
    ("Bandedmail" 75 0 4)
    ("Platemail" 102 0 5)))

(defconstant +rings+
  '(("None" 0 0 0)
    ("Damage +1" 25 1 0)
    ("Damage +2" 50 2 0)
    ("Damage +3" 100 3 0)
    ("Defense +1" 20 0 1)
    ("Defense +2" 40 0 2)
    ("Defense +3" 80 0 3)))

(defun parse-stat-line (line)
  (parse-integer (subseq line (+ (position #\: line) 2))))

(defun main ()
  (let* ((boss-hp 0)
         (boss-damage 0)
         (boss-armor 0)
         (player-hp 100)
         (min-gold most-positive-fixnum))

    (with-open-file (stream "input.txt" :direction :input)
      (setf boss-hp (parse-stat-line (read-line stream)))
      (setf boss-damage (parse-stat-line (read-line stream)))
      (setf boss-armor (parse-stat-line (read-line stream))))

    (dolist (weapon +weapons+)
      (dolist (armor-choice +armor+)
        (loop for i from 0 below (length +rings+)
              for ring1 = (nth i +rings+)
              do (loop for j from (+ i 1) below (length +rings+)
                       for ring2 = (nth j +rings+)
                       do
                         (let* ((current-cost (+ (cadr weapon) (cadr armor-choice) (cadr ring1) (cadr ring2)))
                                (current-player-damage (+ (caddr weapon) (caddr armor-choice) (caddr ring1) (caddr ring2)))
                                (current-player-armor (+ (cadddr weapon) (cadddr armor-choice) (cadddr ring1) (cadddr ring2)))

                                (effective-player-damage (max 1 (- current-player-damage boss-armor)))
                                (effective-boss-damage (max 1 (- boss-damage current-player-armor)))

                                (player-turns (ceiling boss-hp effective-player-damage))
                                (boss-turns (ceiling player-hp effective-boss-damage)))

                           (when (<= player-turns boss-turns)
                             (setf min-gold (min min-gold current-cost))))))))

    (format t "~a~%" min-gold)))

(main)
