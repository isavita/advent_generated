(ns day2
  (:require [clojure.string :as str]))

(defn count-letters [s]
  (frequencies s))

(defn has-exactly [counts n]
  (some (fn [[_ v]] (= v n)) counts))

(defn checksum [ids]
  (let [twos (count (filter #(has-exactly (count-letters %) 2) ids))
        threes (count (filter #(has-exactly (count-letters %) 3) ids))]
    (* twos threes)))

(defn find-close-ids [ids]
  (let [ids (sort ids)]
    (loop [ids ids]
      (let [id (first ids)
            rest-ids (rest ids)]
        (if (empty? rest-ids)
          nil
          (let [diffs (map (fn [c1 c2] (if (= c1 c2) nil c1)) id (first rest-ids))]
            (if (= 1 (count (remove nil? diffs)))
              [(str/join (remove nil? diffs)) (str/join (remove nil? (map (fn [c1 c2] (if (= c1 c2) c1 nil)) id (first rest-ids))))]
              (recur rest-ids))))))))

(defn -main []
  (let [ids (str/split (slurp "input.txt") #"\n")]
    (println "Checksum:" (checksum ids))
    (let [close-ids (find-close-ids ids)]
      (println "Common letters:" (first close-ids) "and" (second close-ids)))))

(-main)