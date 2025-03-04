
(ns jumbled-seven-segment.core
  (:require [clojure.string :as str]))

(defn alphabetize-string [s]
  (apply str (sort s)))

(defn check-string-overlap [larger smaller]
  (let [large-set (set larger)]
    (every? #(contains? large-set %) smaller)))

(defn parse-line [line]
  (let [parts (re-seq #"[a-g]+" line)]
    (if (= (count parts) 14)
      (map alphabetize-string parts)
      (throw (ex-info "Invalid input line" {:line line})))))

(defn decode-digits [patterns]
  (let [one (first (filter #(= 2 (count %)) patterns))
        four (first (filter #(= 4 (count %)) patterns))
        seven (first (filter #(= 3 (count %)) patterns))
        eight (first (filter #(= 7 (count %)) patterns))
        
        zero-three-nine (filter #(check-string-overlap % one) (remove #{one four seven eight} patterns))
        three (first (filter #(= 5 (count %)) zero-three-nine))
        nine (first (filter #(check-string-overlap % four) (remove #{three} zero-three-nine)))
        zero (first (remove #{three nine} zero-three-nine))

        remaining (remove #{zero one three four seven eight nine} patterns)
        six (first (filter #(= 6 (count %)) remaining))
        five (first (filter #(check-string-overlap nine %) (remove #{six} remaining)))
        two (first (remove #{six five} remaining))]
    
    {zero 0, one 1, two 2, three 3, four 4,
     five 5, six 6, seven 7, eight 8, nine 9}))

(defn calculate-output [line]
  (let [patterns (take 10 line)
        output (drop 10 line)
        digit-map (decode-digits patterns)]
    (reduce (fn [acc digit] (+ (* 10 acc) (get digit-map digit))) 0 output)))

(defn solve [input-data]
  (->> (str/split-lines input-data)
       (map parse-line)
       (map calculate-output)
       (reduce +)))

(defn main []
  (let [input-text (slurp "input.txt")
        answer (solve input-text)]
    (println answer)))

(main)
