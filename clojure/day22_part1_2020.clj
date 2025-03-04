
(ns crab-combat
  (:require [clojure.string :as str]))

(defn parse-decks [input]
  (let [[deck1 deck2] (str/split input #"\n\n")]
    [(mapv #(Integer/parseInt %) (rest (str/split-lines deck1)))
     (mapv #(Integer/parseInt %) (rest (str/split-lines deck2)))]))

(defn play-round [decks]
  (let [[deck1 deck2] decks
        card1 (first deck1)
        card2 (first deck2)]
    (cond
      (> card1 card2) [(conj (vec (rest deck1)) card1 card2) (vec (rest deck2))]
      (< card1 card2) [(vec (rest deck1)) (conj (vec (rest deck2)) card2 card1)]
      :else (throw (Exception. "Cards are equal!")))))

(defn game-over? [decks]
  (or (empty? (first decks)) (empty? (second decks))))

(defn calculate-score [deck]
  (->> deck
       reverse
       (map-indexed #(* (inc %1) %2))
       (reduce +)))

(defn -main []
  (let [input (slurp "input.txt")
        initial-decks (parse-decks input)]
    (loop [decks initial-decks]
      (if (game-over? decks)
        (let [winner (if (empty? (first decks)) (second decks) (first decks))]
          (println (calculate-score winner)))
        (recur (play-round decks))))))

(-main)
