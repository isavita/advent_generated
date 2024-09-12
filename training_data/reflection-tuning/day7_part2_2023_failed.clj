(ns day07
  (:require [clojure.string :as str]))

(def card-ranks-p1 {\A 14 \K 13 \Q 12 \J 11 \T 10 \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2})
(def card-ranks-p2 {\A 14 \K 13 \Q 12 \T 10 \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2 \J 1})

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

(defn best-hand-type [hand]
  (if (str/includes? hand "J")
    (apply max (map #(hand-type (str/replace hand #"J" (str %)))
                    "AKQT98765432"))
    (hand-type hand)))

(defn compare-hands [ranks hand1 hand2]
  (let [type1 (best-hand-type hand1)
        type2 (best-hand-type hand2)]
    (if (= type1 type2)
      (or (some #(compare (ranks %1) (ranks %2)) (map vector hand1 hand2)) 0)
      (- type1 type2))))

(defn parse-input [input]
  (map #(let [[hand bid] (str/split % #" ")]
          [hand (parse-long bid)])
       (str/split-lines input)))

(defn total-winnings [hands compare-fn]
  (->> hands
       (sort-by first compare-fn)
       (map-indexed (fn [idx [_ bid]] (* (inc idx) bid)))
       (reduce +)))

(defn solve [input]
  (let [hands (parse-input input)]
    [(total-winnings hands (partial compare-hands card-ranks-p1))
     (total-winnings hands (partial compare-hands card-ranks-p2))]))

;; Example usage:
;; (solve (slurp "input.txt"))
