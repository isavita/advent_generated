
(require '[clojure.string :as str])

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(Integer/parseInt %))))

(defn target-weight [weights]
  (/ (reduce + weights) 3))

(defn quantum-entanglement [group]
  (reduce * group))

(defn find-combinations [items target n]
  (cond
    (zero? n) (if (zero? target) [[]] [])
    (empty? items) []
    :else (let [first-item (first items)
                rest-items (rest items)]
            (concat
             (when (>= target first-item)
               (->> (find-combinations rest-items (- target first-item) (dec n))
                    (map #(cons first-item %))))
             (find-combinations rest-items target n)))))

(defn solve [weights]
  (let [target (target-weight weights)
        min-group-size (loop [n 1]
                         (if (not-empty (find-combinations weights target n))
                           n
                           (recur (inc n))))
        valid-groups (->> (find-combinations weights target min-group-size)
                          (sort-by quantum-entanglement))]
    (quantum-entanglement (first valid-groups))))

(defn -main [& args]
  (let [input (slurp "input.txt")
        weights (parse-input input)
        result (solve weights)]
    (println result)))

(-main)
