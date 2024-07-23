
(ns syntax-scoring
  (:require [clojure.string :as str]))

(def opening-chars {\( \), \[ \], \{ \}, \< \>})
(def closing-chars (into {} (map (fn [[k v]] [v k]) opening-chars)))

(defn score-corrupted [char]
  (case char
    \) 3
    \] 57
    \} 1197
    \> 25137))

(defn score-autocomplete [char]
  (case char
    \) 1
    \] 2
    \} 3
    \> 4))

(defn check-line [line]
  (loop [stack [] chars (seq line)]
    (if (empty? chars)
      [:incomplete stack]
      (let [char (first chars)]
        (if (contains? opening-chars char)
          (recur (conj stack char) (rest chars))
          (if (and (not (empty? stack))
                   (= (closing-chars char) (peek stack)))
            (recur (pop stack) (rest chars))
            [:corrupted char]))))))

(defn process-input [lines]
  (let [results (map check-line lines)]
    (let [corrupted (filter #(= (first %) :corrupted) results)
          incomplete (filter #(= (first %) :incomplete) results)]
      {:corrupted corrupted
       :incomplete incomplete})))

(defn calculate-syntax-error-score [corrupted]
  (reduce + (map (comp score-corrupted second) corrupted)))

(defn complete-line [stack]
  (map opening-chars (reverse stack)))

(defn calculate-autocomplete-score [completion]
  (reduce (fn [score char]
            (+ (* score 5) (score-autocomplete char)))
          0 completion))

(defn middle-autocomplete-score [incomplete]
  (let [completion-scores (map (comp calculate-autocomplete-score complete-line second) incomplete)]
    (let [sorted-scores (sort completion-scores)]
      (nth sorted-scores (quot (count sorted-scores) 2)))))

(defn main []
  (let [lines (str/split-lines (slurp "input.txt"))
        {corrupted :corrupted
         incomplete :incomplete} (process-input lines)]
    (let [syntax-error-score (calculate-syntax-error-score corrupted)
          middle-score (middle-autocomplete-score incomplete)]
      (println "Total Syntax Error Score:" syntax-error-score)
      (println "Middle Autocomplete Score:" middle-score))))

(main)
