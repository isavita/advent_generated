
(ns camel-cards
  (:require [clojure.string :as str]))

(def card-strength
  (zipmap "23456789TJQKA" (range)))

(defn hand-type [hand]
  (let [counts (vals (frequencies hand))]
    (cond
      (some #{5} counts) :five-of-a-kind
      (some #{4} counts) :four-of-a-kind
      (and (some #{3} counts) (some #{2} counts)) :full-house
      (some #{3} counts) :three-of-a-kind
      (= 2 (count (filter #{2} counts))) :two-pair
      (some #{2} counts) :one-pair
      :else :high-card)))

(defn compare-hands [hand1 hand2]
  (let [type1 (hand-type hand1)
        type2 (hand-type hand2)
        type-order [:high-card :one-pair :two-pair :three-of-a-kind :full-house :four-of-a-kind :five-of-a-kind]]
    (if (= type1 type2)
      (loop [[c1 & r1] hand1
             [c2 & r2] hand2]
        (if (nil? c1)
          0
          (let [strength-diff (- (card-strength c1) (card-strength c2))]
            (if (not= strength-diff 0)
              strength-diff
              (recur r1 r2)))))
      (- (.indexOf type-order type1) (.indexOf type-order type2)))))

(defn calculate-winnings [hands]
  (->> hands
       (sort-by first compare-hands)
       (map-indexed (fn [idx [_ bid]] (* (inc idx) bid)))
       (reduce +)))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        hands (map #(let [[hand bid] (str/split % #" ")] [hand (parse-long bid)]) lines)]
    (println (calculate-winnings hands))))

(-main)
