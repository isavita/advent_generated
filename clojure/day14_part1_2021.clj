
(defn parse-input [file]
  (let [[polymer & rules] (-> file slurp (clojure.string/split #"\n"))
        rules (->> rules
                   (remove clojure.string/blank?)
                   (map #(clojure.string/split % #" -> "))
                   (into {}))]
    {:polymer polymer :rules rules}))

(defn apply-insertion [polymer rules]
  (loop [i 0
         new-polymer []]
    (if (>= i (dec (count polymer)))
      (apply str (conj new-polymer (nth polymer (dec (count polymer)))))
      (let [pair (subs polymer i (+ i 2))
            insert (get rules pair)]
        (recur (inc i)
               (if insert
                 (conj new-polymer (nth polymer i) insert)
                 (conj new-polymer (nth polymer i))))))))

(defn count-elements [polymer]
  (frequencies polymer))

(defn min-max [counts]
  (let [vals (vals counts)]
    [(apply min vals) (apply max vals)]))

(defn solve [file]
  (let [{:keys [polymer rules]} (parse-input file)
        final-polymer (nth (iterate #(apply-insertion % rules) polymer) 10)
        counts (count-elements final-polymer)
        [min max] (min-max counts)]
    (- max min)))

(println (solve "input.txt"))
