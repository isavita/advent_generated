
(defn parse-rule [line]
  (let [[_ name r1s r1e r2s r2e] (re-matches #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" line)]
    {:name name
     :ranges (map (fn [s e] [(Integer/parseInt s) (Integer/parseInt e)]) [r1s r1e] [r2s r2e])}))

(defn valid? [value ranges]
  (some (fn [[start end]] (and (>= value start) (<= value end))) ranges))

(defn valid-for-any-rule? [value rules]
  (some #(valid? value (:ranges %)) rules))

(defn solve []
  (let [lines (line-seq (java.io.BufferedReader. (java.io.FileReader. "input.txt")))
        rules (->> lines
                   (take-while #(not (or (empty? %) (re-matches #"your ticket:.*" %))))
                   (remove empty?)
                   (map parse-rule))
        tickets (->> lines
                     (drop-while #(not (re-matches #"nearby tickets:.*" %)))
                     rest
                     (remove empty?)
                     (map #(map (fn [x] (Integer/parseInt x)) (clojure.string/split % #","))))
        error-rate (->> tickets
                        (apply concat)
                        (remove #(valid-for-any-rule? % rules))
                        (reduce + 0))]
    (println error-rate)))

(solve)
