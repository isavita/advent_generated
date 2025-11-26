
(ns recursive-combat
  (:require [clojure.string :as str]))

(defn deck [cards] cards)
(defn copy [n deck] (vec (take n deck)))
(defn score [deck]
  (reduce + (map * (range (count deck) 0 -1) deck)))

(defn play-recursive-combat [p1 p2]
  (loop [p1 p1
         p2 p2
         seen #{}]
    (cond
      (empty? p1) [[] p2]
      (empty? p2) [p1 []]
      (seen [p1 p2]) [p1 []]
      :else
      (let [c1 (first p1)
            c2 (first p2)
            r1 (rest p1)
            r2 (rest p2)
            p1-win? (if (and (>= (count r1) c1) (>= (count r2) c2))
                      (let [[sub1 _] (play-recursive-combat (copy c1 r1) (copy c2 r2))]
                        (seq sub1))
                      (> c1 c2))]
        (recur (if p1-win? (conj (vec r1) c1 c2) (vec r1))
               (if p1-win? (vec r2) (conj (vec r2) c2 c1))
               (conj seen [p1 p2]))))))

(defn parse-input [lines]
  (let [blocks (str/split (str/join "\n" lines) #"\n\s*\n")
        parse-deck (fn [s] (deck (mapv #(Long/parseLong %) (rest (str/split-lines s)))))]
    [(parse-deck (first blocks)) (parse-deck (second blocks))]))

(defn -main []
  (let [[p1 p2] (parse-input (str/split-lines (slurp "input.txt")))
        [w _] (play-recursive-combat p1 p2)
        winner (if (seq w) w p2)]
    (println (score winner))))

(-main)
