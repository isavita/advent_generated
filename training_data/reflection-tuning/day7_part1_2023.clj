(ns camel-cards
  (:require [clojure.string :as str]))

(def card-values
  {\A 14, \K 13, \Q 12, \J 11, \T 10, \9 9, \8 8, \7 7, \6 6, \5 5, \4 4, \3 3, \2 2})

(defn hand-type [hand]
  (let [freqs (frequencies hand)
        counts (sort > (vals freqs))]
    (case counts
      [5] 7     ; Five of a kind
      [4 1] 6   ; Four of a kind
      [3 2] 5   ; Full house
      [3 1 1] 4 ; Three of a kind
      [2 2 1] 3 ; Two pair
      [2 1 1 1] 2 ; One pair
      1)))      ; High card

(defn compare-hands [hand1 hand2]
  (let [type1 (hand-type hand1)
        type2 (hand-type hand2)]
    (if (not= type1 type2)
      (- type1 type2)
      (loop [cards1 hand1
             cards2 hand2]
        (if (empty? cards1)
          0
          (let [comp (- (card-values (first cards1))
                        (card-values (first cards2)))]
            (if (zero? comp)
              (recur (rest cards1) (rest cards2))
              comp)))))))

(defn parse-input [line]
  (let [[hand bid] (str/split line #"\s+")]
    [hand (Integer/parseInt bid)]))

(defn calculate-winnings [hands-with-bids]
  (->> hands-with-bids
       (sort-by first compare-hands)
       (map-indexed (fn [idx [_ bid]] (* (inc idx) bid)))
       (reduce +)))

(defn solve-camel-cards []
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-input)
       calculate-winnings))

(println (solve-camel-cards))
