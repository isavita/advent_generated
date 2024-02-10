
(defn count-yes-answers [group]
  (count (apply str (distinct (apply str group)))))

(defn sum-yes-answers [groups]
  (reduce + (map count-yes-answers groups)))

(defn read-input []
  (-> "input.txt"
      slurp
      (clojure.string/split-lines)))

(def groups (partition-by empty? (read-input)))

(println (sum-yes-answers groups))
