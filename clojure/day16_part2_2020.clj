
(ns ticket-translation
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-rules [section]
  (into {} (for [line (str/split-lines section)
                :let [[field ranges] (str/split line #": ")
                      parts (str/split ranges #" or ")
                      ranges (mapv (fn [p] (let [v (str/split p #"-")]
                                            [(Long/parseLong (v 0)) (Long/parseLong (v 1))])) parts)]]
            [field ranges])))

(defn parse-tickets [section]
  (mapv (fn [l] (mapv #(Long/parseLong %) (str/split l #","))) (rest (str/split-lines section))))

(defn valid-for-any? [value rules]
  (some (fn [ranges] (some (fn [[start end]] (<= start value end)) ranges)) (vals rules)))

(defn field-order [rules tickets]
  (let [n (count (first tickets))
        possible (vec (for [i (range n)
                           :let [col (mapv #(nth % i) tickets)
                                 valid-fields (for [[field ranges] rules
                                                   :when (every? (fn [v] (some (fn [[s e]] (<= s v e)) ranges)) col)]
                                                field)]]
                       (set valid-fields)))
        order (atom (vec (repeat n nil)))
        used (atom #{})]
    (while (< (count @used) n)
      (dotimes [i n]
        (when-let [only (first (set/difference (possible i) @used))]
          (when (= 1 (count (set/difference (possible i) @used)))
            (swap! order assoc i only)
            (swap! used conj only)))))
    @order))

(defn -main []
  (let [input (slurp "input.txt")
        [rules-section my-section nearby-section] (str/split input #"\n\n")
        rules (parse-rules rules-section)
        nearby (parse-tickets nearby-section)
        my-ticket (first (parse-tickets my-section))
        error-rate (transduce (comp (mapcat identity)
                                   (filter #(not (valid-for-any? % rules)))
                                   (map long)) + nearby)
        valid-tickets (filterv (fn [t] (every? #(valid-for-any? % rules) t)) nearby)
        order (field-order rules valid-tickets)
        departure-indices (keep-indexed (fn [i f] (when (and f (str/starts-with? f "departure")) i)) order)
        departure-product (transduce (map #(nth my-ticket %)) * departure-indices)]
    (println error-rate)
    (println departure-product)))

(-main)
