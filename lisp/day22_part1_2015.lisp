
(defstruct game-state
  player-hp
  player-mana
  boss-hp
  boss-damage
  shield-timer
  poison-timer
  recharge-timer
  mana-spent)

(defun get-value-from-line (line)
  (parse-integer (subseq line (+ (position #\: line) 2))))

(defun min-mana-to-win (initial-boss-hp initial-boss-damage)
  (let ((min-mana most-positive-fixnum))
    (labels ((simulate (state player-turn)
               (when (>= (game-state-mana-spent state) min-mana)
                 (return-from simulate))
               (when (<= (game-state-boss-hp state) 0)
                 (setf min-mana (game-state-mana-spent state))
                 (return-from simulate))
               (when (<= (game-state-player-hp state) 0)
                 (return-from simulate))

               (let ((s (copy-game-state state)))
                 (when (> (game-state-shield-timer s) 0)
                   (decf (game-state-shield-timer s)))
                 (when (> (game-state-poison-timer s) 0)
                   (decf (game-state-boss-hp s) 3)
                   (decf (game-state-poison-timer s)))
                 (when (> (game-state-recharge-timer s) 0)
                   (incf (game-state-player-mana s) 101)
                   (decf (game-state-recharge-timer s)))

                 (when (>= (game-state-mana-spent s) min-mana) ; Re-check after effects
                   (return-from simulate))
                 (when (<= (game-state-boss-hp s) 0)
                   (setf min-mana (game-state-mana-spent s))
                   (return-from simulate))
                 (when (<= (game-state-player-hp s) 0) ; Re-check after effects if any caused damage
                   (return-from simulate))

                 (if player-turn
                     (progn
                       (when (>= (game-state-player-mana s) 53)
                         (let ((ns (copy-game-state s)))
                           (decf (game-state-player-mana ns) 53)
                           (incf (game-state-mana-spent ns) 53)
                           (decf (game-state-boss-hp ns) 4)
                           (simulate ns nil)))
                       (when (>= (game-state-player-mana s) 73)
                         (let ((ns (copy-game-state s)))
                           (decf (game-state-player-mana ns) 73)
                           (incf (game-state-mana-spent ns) 73)
                           (decf (game-state-boss-hp ns) 2)
                           (incf (game-state-player-hp ns) 2)
                           (simulate ns nil)))
                       (when (and (>= (game-state-player-mana s) 113)
                                  (= (game-state-shield-timer s) 0))
                         (let ((ns (copy-game-state s)))
                           (decf (game-state-player-mana ns) 113)
                           (incf (game-state-mana-spent ns) 113)
                           (setf (game-state-shield-timer ns) 6)
                           (simulate ns nil)))
                       (when (and (>= (game-state-player-mana s) 173)
                                  (= (game-state-poison-timer s) 0))
                         (let ((ns (copy-game-state s)))
                           (decf (game-state-player-mana ns) 173)
                           (incf (game-state-mana-spent ns) 173)
                           (setf (game-state-poison-timer ns) 6)
                           (simulate ns nil)))
                       (when (and (>= (game-state-player-mana s) 229)
                                  (= (game-state-recharge-timer s) 0))
                         (let ((ns (copy-game-state s)))
                           (decf (game-state-player-mana ns) 229)
                           (incf (game-state-mana-spent ns) 229)
                           (setf (game-state-recharge-timer ns) 5)
                           (simulate ns nil))))
                     (let ((damage (game-state-boss-damage s)))
                       (when (> (game-state-shield-timer s) 0)
                         (decf damage 7))
                       (when (< damage 1)
                         (setf damage 1))
                       (decf (game-state-player-hp s) damage)
                       (simulate s t))))))
      (let ((initial-game-state (make-game-state
                                 :player-hp 50
                                 :player-mana 500
                                 :boss-hp initial-boss-hp
                                 :boss-damage initial-boss-damage
                                 :shield-timer 0
                                 :poison-timer 0
                                 :recharge-timer 0
                                 :mana-spent 0)))
        (simulate initial-game-state t)
        min-mana))))

(defun main ()
  (let (boss-hp boss-damage)
    (with-open-file (stream "input.txt" :direction :input)
      (setf boss-hp (get-value-from-line (read-line stream)))
      (setf boss-damage (get-value-from-line (read-line stream))))
    (print (min-mana-to-win boss-hp boss-damage))))

(main)
