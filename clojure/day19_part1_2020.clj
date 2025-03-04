
(ns day19
  (:require [clojure.string :as str]))

(defn parse-rule [rule-str]
  (let [[id rest] (str/split rule-str #": ")
        id (parse-long id)]
    (if (str/includes? rest "\"")
      [id (subs rest 1 (dec (count rest)))]
      [id (mapv #(mapv parse-long (str/split % #" "))
                (str/split rest #" \| "))])))

(defn parse-input [input]
  (let [[rules-str messages-str] (str/split input #"\n\n")
        rules (->> (str/split-lines rules-str)
                   (map parse-rule)
                   (into {}))]
    [rules (str/split-lines messages-str)]))

(defn matches? [rules rule-id message pos]
    (let [rule (get rules rule-id)]
      (if (string? rule)
        (if (and (< pos (count message)) (= (str (nth message pos)) rule))
          [(inc pos)]
          [])
        (mapcat (fn [option]
                  (reduce (fn [acc sub-rule-id]
                            (mapcat #(matches? rules sub-rule-id message %) acc))
                          [pos]
                          option))
                rule))))


(defn solve [input]
  (let [[rules messages] (parse-input input)
        valid-end-positions (map #(matches? rules 0 % 0) messages)]
    (count (filter #(= (count %) 1)
                 (map #(filter (fn [end-pos] (= end-pos (count %2))) %) valid-end-positions messages)))))


(defn -main []
  (let [input (slurp "input.txt")]
      (println (solve input))))

(-main)
