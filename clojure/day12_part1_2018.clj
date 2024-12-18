
(defn parse-input [lines]
  (let [[initial-line & rule-lines] lines
        initial-state (-> initial-line (clojure.string/split #": ") second)
        rules (->> rule-lines
                   (filter #(clojure.string/includes? % "=>"))
                   (map #(clojure.string/split % #" => "))
                   (into {} (map (fn [[k v]] [k (first v)]))))
        state (->> (map-indexed vector initial-state)
                   (filter (fn [[_ c]] (= c \#)))
                   (map first)
                   (into #{}))]
    {:initial-state initial-state :rules rules :state state}))

(defn next-gen [state rules]
  (let [min-pot (apply min state)
        max-pot (apply max state)]
    (->> (range (- min-pot 2) (+ max-pot 3))
         (filter (fn [i]
                   (let [pattern (->> (range (- i 2) (+ i 3))
                                     (map (fn [j] (if (contains? state j) \# \.)))
                                     (apply str))]
                     (= \# (get rules pattern \.)))))
         (into #{}))))

(defn solve []
  (let [{:keys [rules state]} (-> "input.txt"
                                  slurp
                                  clojure.string/split-lines
                                  parse-input)
        final-state (->> (iterate #(next-gen % rules) state)
                         (drop 20)
                         first)]
    (reduce + final-state)))

(println (solve))
