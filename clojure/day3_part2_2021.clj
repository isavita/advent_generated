(ns binary-diagnostic.part-two
  (:require [clojure.string :as str]))

(defn read-binary-input [filename]
  (slurp filename))

(defn parse-input [input]
  (str/split-lines input))

(defn filter-by-criteria [lines bit-index criteria-fn tie-breaker]
  (let [bit-counts (frequencies (map #(nth % bit-index) lines))
        most-common (if (= (get bit-counts \1) (get bit-counts \0))
                      tie-breaker
                      (criteria-fn bit-counts))]
    (filter #(= (nth % bit-index) most-common) lines)))

(defn find-rating [lines criteria-fn tie-breaker]
  (loop [remaining-lines lines
         bit-index 0]
    (if (or (= 1 (count remaining-lines)) (>= bit-index (count (first remaining-lines))))
      (first remaining-lines)
      (let [filtered-lines (filter-by-criteria remaining-lines bit-index criteria-fn tie-breaker)]
        (recur filtered-lines (inc bit-index))))))

(defn calculate-life-support-rating [filename]
  (let [input (read-binary-input filename)
        lines (parse-input input)
        oxygen-generator-rating (find-rating lines
                                              (fn [counts] (if (>= (get counts \1 0) (get counts \0 0)) \1 \0))
                                              \1)
        co2-scrubber-rating (find-rating lines
                                         (fn [counts] (if (< (get counts \1 0) (get counts \0 0)) \1 \0))
                                         \0)
        oxygen-decimal (Integer/parseInt oxygen-generator-rating 2)
        co2-decimal (Integer/parseInt co2-scrubber-rating 2)]
    (* oxygen-decimal co2-decimal)))

(println (calculate-life-support-rating "input.txt"))

