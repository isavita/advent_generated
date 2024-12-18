
(defn parse-input [file]
  (let [lines (line-seq (clojure.java.io/reader file))]
    (let [[_ boss-hp] (re-matches #"Hit Points: (\d+)" (first lines))
          [_ boss-damage] (re-matches #"Damage: (\d+)" (second lines))]
      {:boss-hp (Integer/parseInt boss-hp)
       :boss-damage (Integer/parseInt boss-damage)})))

(defn apply-effects [state]
  (let [state (cond-> state
                      (> (:shield-timer state) 0) (update :shield-timer dec)
                      (> (:poison-timer state) 0) (-> (update :boss-hp - 3) (update :poison-timer dec))
                      (> (:recharge-timer state) 0) (-> (update :player-mana + 101) (update :recharge-timer dec)))]
    state))

(defn boss-turn [state]
  (let [damage (max 1 (- (:boss-damage state) (if (> (:shield-timer state) 0) 7 0)))]
    (update state :player-hp - damage)))

(defn simulate [initial-state]
  (let [min-mana (atom Integer/MAX_VALUE)]
    (letfn [(recur-sim [state player-turn]
              (when (< (:mana-spent state) @min-mana)
                (cond
                  (<= (:boss-hp state) 0) (reset! min-mana (:mana-spent state))
                  (<= (:player-hp state) 0) nil
                  :else
                  (let [state (if player-turn (update state :player-hp dec) state)
                        state (if (<= (:player-hp state) 0) nil (apply-effects state))]
                    (when state
                      (if (not player-turn)
                        (recur-sim (boss-turn state) true)
                        (do
                          (when (>= (:player-mana state) 53)
                            (recur-sim (-> state (update :player-mana - 53) (update :mana-spent + 53) (update :boss-hp - 4)) false))
                          (when (>= (:player-mana state) 73)
                            (recur-sim (-> state (update :player-mana - 73) (update :mana-spent + 73) (update :boss-hp - 2) (update :player-hp + 2)) false))
                          (when (and (>= (:player-mana state) 113) (zero? (:shield-timer state)))
                            (recur-sim (-> state (update :player-mana - 113) (update :mana-spent + 113) (assoc :shield-timer 6)) false))
                          (when (and (>= (:player-mana state) 173) (zero? (:poison-timer state)))
                            (recur-sim (-> state (update :player-mana - 173) (update :mana-spent + 173) (assoc :poison-timer 6)) false))
                          (when (and (>= (:player-mana state) 229) (zero? (:recharge-timer state)))
                            (recur-sim (-> state (update :player-mana - 229) (update :mana-spent + 229) (assoc :recharge-timer 5)) false)))))))))]
      (recur-sim (assoc initial-state :player-hp 50 :player-mana 500 :shield-timer 0 :poison-timer 0 :recharge-timer 0 :mana-spent 0) true)
    @min-mana)))

(defn solve []
  (let [input (parse-input "input.txt")]
    (simulate input)))

(println (solve))
