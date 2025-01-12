
(defn simulate [state player-turn min-mana]
  (if (>= (:mana-spent state) @min-mana)
    nil
    (let [boss-hp (:boss-hp state)
          player-hp (:player-hp state)]
      (if (<= boss-hp 0)
        (reset! min-mana (:mana-spent state))
        (if (<= player-hp 0)
          nil
          (let [shield-timer (:shield-timer state)
                poison-timer (:poison-timer state)
                recharge-timer (:recharge-timer state)
                state (cond-> state
                        (> shield-timer 0) (update :shield-timer dec)
                        (> poison-timer 0) (-> (update :boss-hp - 3)
                                               (update :poison-timer dec))
                        (> recharge-timer 0) (-> (update :player-mana + 101)
                                                 (update :recharge-timer dec)))]
            (if (not player-turn)
              (let [damage (max 1 (- (:boss-damage state) (if (> (:shield-timer state) 0) 7 0)))
                    new-state (update state :player-hp - damage)]
                (simulate new-state true min-mana))
              (doseq [[cost effect] [[53 #(update % :boss-hp - 4)]
                                      [73 #(-> % (update :boss-hp - 2) (update :player-hp + 2))]
                                      [113 #(if (zero? (:shield-timer %)) (assoc % :shield-timer 6) %)]
                                      [173 #(if (zero? (:poison-timer %)) (assoc % :poison-timer 6) %)]
                                      [229 #(if (zero? (:recharge-timer %)) (assoc % :recharge-timer 5) %)]]]
                (when (>= (:player-mana state) cost)
                  (let [new-state (-> state
                                      (update :player-mana - cost)
                                      (update :mana-spent + cost)
                                      effect)]
                    (simulate new-state false min-mana)))))))))))

(defn min-mana-to-win [initial-state]
  (let [min-mana (atom Integer/MAX_VALUE)]
    (simulate (assoc initial-state :player-hp 50 :player-mana 500) true min-mana)
    @min-mana))

(let [lines (line-seq (java.io.BufferedReader. (java.io.FileReader. "input.txt")))
      boss-hp (Integer/parseInt (second (clojure.string/split (first lines) #": ")))
      boss-damage (Integer/parseInt (second (clojure.string/split (second lines) #": ")))]
  (println (min-mana-to-win {:boss-hp boss-hp :boss-damage boss-damage :shield-timer 0 :poison-timer 0 :recharge-timer 0 :mana-spent 0})))
