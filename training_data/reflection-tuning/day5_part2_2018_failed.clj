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

(defn solve-part1 [input]
  (count (react-polymer input)))

(defn solve-part2 [input]
  (let [unit-types (set (map str/lower-case input))]
    (->> unit-types
         (pmap (fn [unit]
                 (let [filtered (remove #(= (str/lower-case %) unit) input)]
                   (count (react-polymer filtered)))))
         (apply min))))

(defn solve [input]
  (let [part1 (solve-part1 input)
        part2 (solve-part2 input)]
    [part1 part2]))

;; Example usage:
;; (solve "dabAcCaCBAcCcaDA")
