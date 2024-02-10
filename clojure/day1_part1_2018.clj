
(def input (slurp "input.txt"))

(defn solve [input]
  (reduce + (map #(Integer/parseInt %) (clojure.string/split input #"\n"))))

(println (solve input))
