
(ns aoc.day12
  (:gen-class))

(defn sum-json
  "Recursively sum all numbers in a JSON-like Clojure structure.
   If ignore-red? is true, any map containing the string \"red\" as a value
   is ignored completely."
  [x ignore-red?]
  (cond
    (number? x) x
    (vector? x) (reduce + 0 (map #(sum-json % ignore-red?) x))
    (map? x) (if (and ignore-red? (some #{\"red\"} (vals x)))
               0
               (reduce + 0 (map #(sum-json % ignore-red?) (vals x))))
    :else 0))

(defn -main [& _]
  (let [raw (slurp "input.txt")
        data (clojure.data.json/read-str raw :key-fn keyword)]
    (println (sum-json data false))
    (println (sum-json data true))))
