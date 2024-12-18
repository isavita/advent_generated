
(defn solve []
  (let [[time distance] (->> (slurp "input.txt")
                             (clojure.string/split-lines)
                             (remove clojure.string/blank?)
                             (map #(-> %
                                       (clojure.string/split #":")
                                       second
                                       (clojure.string/replace #" " "")
                                       parse-long)))]
    (->> (range 1 time)
         (map (fn [hold-time] (* hold-time (- time hold-time))))
         (filter #(> % distance))
         count)))

(println (solve))
