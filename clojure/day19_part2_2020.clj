
(ns day19.core
  (:require [clojure.string :as str]))

(defn parse-rule [rule-str]
  (let [[id rest] (str/split rule-str #": ")]
    (if (str/includes? rest "\"")
      [(Integer/parseInt id) (str/replace rest "\"" "")]
      [(Integer/parseInt id)
       (mapv #(mapv (fn [x] (Integer/parseInt x)) (str/split % #" "))
             (str/split rest #" \| "))])))

(defn parse-input [input]
  (let [[rules-str messages-str] (str/split input #"\n\n")
        rules (into {} (map parse-rule (str/split-lines rules-str)))
        messages (str/split-lines messages-str)]
    [rules messages]))

(defn- matches? [rules rule-id message start-index]
  (if (string? (rules rule-id))
    (if (and (< start-index (count message))
             (= (str (nth message start-index)) (rules rule-id)))
      [[(inc start-index)]]
      [])
    (mapcat
     (fn [option]
       (reduce
        (fn [acc next-rule-id]
          (mapcat
           (fn [idx]
             (matches? rules next-rule-id message idx))
           acc))
        [start-index]
        option))
     (rules rule-id))))

(defn solve-part1 [rules messages]
  (count (filter #(some (fn [end-index] (= end-index (count %))) (matches? rules 0 % 0)) messages)))


(defn solve-part2 [rules messages]
  ;; Modify the rules for part 2
  (let [updated-rules (assoc rules 8 [[42] [42 8]] 11 [[42 31] [42 11 31]])]
      (count (filter #(some (fn [end-index] (= end-index (count %))) (matches? updated-rules 0 % 0)) messages))
    )  
)

(defn -main []
  (let [input (slurp "input.txt")
        [rules messages] (parse-input input)]
    (println "Part 1:" (solve-part1 rules messages))
    (println "Part 2:" (solve-part2 rules messages))))

(-main)
