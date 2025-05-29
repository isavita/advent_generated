
(defun item-cost (item) (nth 0 item))
(defun item-damage (item) (nth 1 item))
(defun item-armor (item) (nth 2 item))

(defun stats-hit-points (stats) (nth 0 stats))
(defun stats-damage (stats) (nth 1 stats))
(defun stats-armor (stats) (nth 2 stats))

(defun equipment-weapon (eq) (nth 0 eq))
(defun equipment-armor (eq) (nth 1 eq))
(defun equipment-rings (eq) (nth 2 eq))

(defparameter *weapons*
  '((8 4 0)
    (10 5 0)
    (25 6 0)
    (40 7 0)
    (74 8 0)))

(defparameter *armors*
  '((13 0 1)
    (31 0 2)
    (53 0 3)
    (75 0 4)
    (102 0 5)))

(defparameter *rings*
  (sort '((25 1 0) (50 2 0) (100 3 0) (20 0 1) (40 0 2) (80 0 3))
        #'< :key #'item-cost))

(defun simulate-battle (player boss)
  (let ((player-hp (stats-hit-points player))
        (boss-hp (stats-hit-points boss))
        (player-damage (stats-damage player))
        (boss-damage (stats-damage boss))
        (player-armor (stats-armor player))
        (boss-armor (stats-armor boss)))
    (loop
      (decf boss-hp (max 1 (- player-damage boss-armor)))
      (when (<= boss-hp 0) (return-from simulate-battle t))

      (decf player-hp (max 1 (- boss-damage player-armor)))
      (when (<= player-hp 0) (return-from simulate-battle nil)))))

(defun calculate-cost (equipment)
  (let ((weapon (equipment-weapon equipment))
        (armor (equipment-armor equipment))
        (rings (equipment-rings equipment)))
    (+ (item-cost weapon)
       (if armor (item-cost armor) 0)
       (reduce #'+ rings :initial-value 0 :key #'item-cost))))

(defun generate-equipment ()
  (let ((equipments '()))
    (dolist (weapon *weapons*)
      (dolist (armor (cons nil *armors*))
        (dolist (ring1 *rings*)
          (dolist (ring2 (cons nil *rings*))
            (when (or (null ring2) (< (item-cost ring2) (item-cost ring1)))
              (let ((current-rings (remove nil (list ring1 ring2))))
                (push (list weapon armor current-rings) equipments)))))))
    (nreverse equipments)))

(defun parse-stat-line (line)
  (parse-integer (subseq line (+ (position #\: line) 2))))

(defun main ()
  (let (boss-hit-points boss-damage boss-armor)
    (with-open-file (in "input.txt" :direction :input)
      (setf boss-hit-points (parse-stat-line (read-line in)))
      (setf boss-damage (parse-stat-line (read-line in)))
      (setf boss-armor (parse-stat-line (read-line in))))

    (let* ((boss (list boss-hit-points boss-damage boss-armor))
           (player-base (list 100 0 0))
           (min-cost most-positive-fixnum)
           (max-cost 0))

      (dolist (equipment (generate-equipment))
        (let* ((player-damage (+ (item-damage (equipment-weapon equipment))
                                 (if (equipment-armor equipment) (item-damage (equipment-armor equipment)) 0)
                                 (reduce #'+ (equipment-rings equipment) :initial-value 0 :key #'item-damage)))
               (player-armor (+ (item-armor (equipment-weapon equipment))
                                (if (equipment-armor equipment) (item-armor (equipment-armor equipment)) 0)
                                (reduce #'+ (equipment-rings equipment) :initial-value 0 :key #'item-armor)))
               (player (list (stats-hit-points player-base) player-damage player-armor))
               (cost (calculate-cost equipment)))
          (if (simulate-battle player boss)
              (setf min-cost (min min-cost cost))
              (setf max-cost (max max-cost cost)))))

      (format t "Least amount of gold to win: ~a~%" min-cost)
      (format t "Most amount of gold to lose: ~a~%" max-cost))))

(main)
