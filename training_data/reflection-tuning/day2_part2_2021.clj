(ns advent-of-code.day2
  (:require [clojure.string :as str]))

(defn parse-command [line]
  (let [[cmd val] (str/split line #" ")]
    [(keyword cmd) (Integer/parseInt val)]))

(defn update-position-part1 [{:keys [horizontal depth] :as state} [cmd val]]
  (case cmd
    :forward (update state :horizontal + val)
    :down    (update state :depth + val)
    :up      (update state :depth - val)))

(defn update-position-part2 [{:keys [horizontal depth aim] :as state} [cmd val]]
  (case cmd
    :forward (-> state
                 (update :horizontal + val)
                 (update :depth + (* aim val)))
    :down    (update state :aim + val)
    :up      (update state :aim - val)))

(defn solve-dive [update-fn]
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-command)
       (reduce update-fn {:horizontal 0 :depth 0 :aim 0})
       ((juxt :horizontal :depth))
       (apply *)))

(defn -main []
  (println "Part 1:" (solve-dive update-position-part1))
  (println "Part 2:" (solve-dive update-position-part2)))

(-main)
