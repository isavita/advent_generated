(require '[clojure.string :as str])

(defn react-polymer [polymer]
  (reduce (fn [stack c]
            (if (and (seq stack)
                     (not= c (last stack))
                     (= (str/lower-case c) (str/lower-case (last stack))))
              (pop stack)
              (conj stack c)))
          []
          polymer))

(defn solve-part1 [polymer]
  (count (react-polymer polymer)))

(defn solve-part2 [polymer]
  (let [unit-types (set (map str/lower-case polymer))]
    (->> unit-types
         (pmap (fn [unit]
                 (->> polymer
                      (remove #(= (str/lower-case %) unit))
                      (apply str)
                      react-polymer
                      count)))
         (apply min))))

(defn -main []
  (let [polymer (str/trim (slurp "input.txt"))]
    (println "Part 1:" (solve-part1 polymer))
    (println "Part 2:" (solve-part2 polymer))))

(-main)
