
(defn parse-stat [line]
  (-> line (clojure.string/split #": ") second Integer/parseInt))

(defn read-boss-stats [filename]
  (let [[hp-line dmg-line armor-line] (clojure.string/split-lines (slurp filename))]
    {:hit-points (parse-stat hp-line)
     :damage (parse-stat dmg-line)
     :armor (parse-stat armor-line)}))

(def weapons
  [{:cost 8 :damage 4}
   {:cost 10 :damage 5}
   {:cost 25 :damage 6}
   {:cost 40 :damage 7}
   {:cost 74 :damage 8}])

(def armors
  [{:cost 0 :armor 0}
   {:cost 13 :armor 1}
   {:cost 31 :armor 2}
   {:cost 53 :armor 3}
   {:cost 75 :armor 4}
   {:cost 102 :armor 5}])

(def rings
  [{:cost 0 :damage 0 :armor 0}
   {:cost 25 :damage 1 :armor 0}
   {:cost 50 :damage 2 :armor 0}
   {:cost 100 :damage 3 :armor 0}
   {:cost 20 :damage 0 :armor 1}
   {:cost 40 :damage 0 :armor 2}
   {:cost 80 :damage 0 :armor 3}])

(defn player-wins? [player boss]
  (let [player-damage (max 1 (- (:damage player) (:armor boss)))
        boss-damage (max 1 (- (:damage boss) (:armor player)))
        player-turns (int (Math/ceil (/ (:hit-points boss) player-damage)))
        boss-turns (int (Math/ceil (/ (:hit-points player) boss-damage)))]
    (<= player-turns boss-turns)))

(defn solve []
  (let [boss (read-boss-stats "input.txt")
        min-cost (atom Integer/MAX_VALUE)]
    (doseq [w weapons
            a armors
            r1 (range (count rings))
            r2 (range (inc r1) (count rings))]
      (let [player {:hit-points 100
                    :damage (+ (:damage w) (:damage (nth rings r1)) (:damage (nth rings r2)))
                    :armor (+ (:armor a) (:armor (nth rings r1)) (:armor (nth rings r2)))
                    }
            cost (+ (:cost w) (:cost a) (:cost (nth rings r1)) (:cost (nth rings r2)))]
        (when (and (player-wins? player boss) (< cost @min-cost))
          (reset! min-cost cost))))
    (println @min-cost)))

(solve)
