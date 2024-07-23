
(ns rpg-simulator
  (:require [clojure.java.io :as io]))

(def weapons
  [{:name "Dagger" :cost 8 :damage 4 :armor 0}
   {:name "Shortsword" :cost 10 :damage 5 :armor 0}
   {:name "Warhammer" :cost 25 :damage 6 :armor 0}
   {:name "Longsword" :cost 40 :damage 7 :armor 0}
   {:name "Greataxe" :cost 74 :damage 8 :armor 0}])

(def armors
  [{:name "Leather" :cost 13 :damage 0 :armor 1}
   {:name "Chainmail" :cost 31 :damage 0 :armor 2}
   {:name "Splintmail" :cost 53 :damage 0 :armor 3}
   {:name "Bandedmail" :cost 75 :damage 0 :armor 4}
   {:name "Platemail" :cost 102 :damage 0 :armor 5}])

(def rings
  [{:name "Damage +1" :cost 25 :damage 1 :armor 0}
   {:name "Damage +2" :cost 50 :damage 2 :armor 0}
   {:name "Damage +3" :cost 100 :damage 3 :armor 0}
   {:name "Defense +1" :cost 20 :damage 0 :armor 1}
   {:name "Defense +2" :cost 40 :damage 0 :armor 2}
   {:name "Defense +3" :cost 80 :damage 0 :armor 3}])

(defn read-boss-stats [filename]
  (with-open [rdr (io/reader filename)]
    (let [lines (line-seq rdr)]
      (let [[hp damage armor] (map #(Integer. (re-find #"\d+" %)) lines)]
        {:hp hp :damage damage :armor armor}))))

(defn calculate-damage [attacker defender]
  (max 1 (- (:damage attacker) (:armor defender))))

(defn fight [player boss]
  (loop [player-hp 100
         boss-hp (:hp boss)]
    (if (<= boss-hp 0) 
      true
      (let [player-damage (calculate-damage player boss)
            new-boss-hp (- boss-hp player-damage)]
        (if (<= new-boss-hp 0)
          true
          (let [boss-damage (calculate-damage boss player)
                new-player-hp (- player-hp boss-damage)]
            (if (<= new-player-hp 0)
              false
              (recur new-player-hp new-boss-hp))))))))

(defn item-combinations []
  (for [weapon weapons
        armor (cons nil armors) ; allow no armor
        rings1 rings
        rings2 (cons nil (remove #(= rings1 %) rings))] ; allow one or two rings
    (let [damage (+ (:damage weapon) (:damage (or armor {:damage 0})) 
                    (:damage (or rings1 {:damage 0})) 
                    (:damage (or rings2 {:damage 0})))
          armor-value (+ (:armor (or armor {:armor 0})) 
                         (:armor (or rings1 {:armor 0})) 
                         (:armor (or rings2 {:armor 0})))]
      {:cost (+ (:cost weapon) (or (:cost armor) 0) 
                 (:cost rings1 0) (:cost rings2 0))
       :damage damage
       :armor armor-value})))

(defn find-min-gold-to-win [boss]
  (->> (item-combinations)
       (filter #(fight % boss))
       (map :cost)
       (apply min)))

(defn find-max-gold-to-lose [boss]
  (->> (item-combinations)
       (filter #(not (fight % boss)))
       (map :cost)
       (apply max)))

(defn -main []
  (let [boss (read-boss-stats "input.txt")]
    (println "Minimum gold to win:" (find-min-gold-to-win boss))
    (println "Maximum gold to lose:" (find-max-gold-to-lose boss))))

(-main)
