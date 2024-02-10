
(require '[clojure.string :as str])

(defn react [polymer]
  (loop [units (seq polymer) stack ()]
    (if (empty? units)
      (apply str (reverse stack))
      (let [unit (first units)
            remaining (rest units)]
        (if (and (not (empty? stack))
                 (= (clojure.string/lower-case unit) (clojure.string/lower-case (first stack)))
                 (not= unit (first stack)))
          (recur remaining (rest stack))
          (recur remaining (cons unit stack)))))))

(defn fully-reacted-length [polymer]
  (count (react polymer)))

(defn remove-unit-type [polymer unit-type]
  (str/join (filter #(not= unit-type (clojure.string/lower-case %)) polymer)))

(defn shortest-polymer-length [polymer]
  (->> polymer
       (map clojure.string/lower-case)
       (distinct)
       (map #(fully-reacted-length (remove-unit-type polymer %)))
       (apply min)))

(def input (slurp "input.txt"))
(println (shortest-polymer-length input))
