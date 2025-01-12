
(defn parse-input [lines]
  (let [[initial-line & rule-lines] lines
        initial-state (-> initial-line (clojure.string/split #": ") second)
        rules (->> rule-lines
                   (filter #(clojure.string/includes? % "=>"))
                   (map #(clojure.string/split % #" => "))
                   (into {} (map (fn [[k v]] [k (first v)]))))
        state (->> (map-indexed vector initial-state)
                   (filter (fn [[_ c]] (= c \#)))
                   (into {} (map (fn [[i _]] [i \#]))))]
    {:initial-state initial-state :rules rules :state state}))

(defn min-max-keys [m]
  (if (empty? m)
    [0 0]
    (let [keys (keys m)]
      [(apply min keys) (apply max keys)])))

(defn state-pattern [state]
  (let [[min-pot max-pot] (min-max-keys state)
        pattern (->> (range min-pot (inc max-pot))
                     (map #(if (get state % nil) \# \.))
                     (apply str))
        sum (->> (keys state) (reduce + 0))]
    [pattern sum]))

(defn next-generation [state rules]
  (let [[min-pot max-pot] (min-max-keys state)]
    (->> (range (- min-pot 2) (+ max-pot 3))
         (reduce (fn [new-state i]
                   (let [pattern (->> (range (- i 2) (+ i 3))
                                      (map #(if (get state % nil) \# \.))
                                      (apply str))
                         next-plant (get rules pattern nil)]
                     (if (= next-plant \#)
                       (assoc new-state i \#)
                       new-state)))
                 {}))))

(defn solve []
  (let [lines (-> "input.txt" slurp clojure.string/split-lines)
        {:keys [rules state]} (parse-input lines)
        limit 50000000000]
    (loop [generation 0
           current-state state
           previous-pattern ""
           previous-sum 0
           offset 0]
      (if (= generation limit)
        (println previous-sum)
        (let [new-state (next-generation current-state rules)
              [current-pattern current-sum] (state-pattern new-state)]
          (if (= current-pattern previous-pattern)
            (let [offset (- current-sum previous-sum)
                  remaining-generations (- limit generation 1)
                  final-sum (+ current-sum (* offset remaining-generations))]
              (println final-sum)
              )
            (recur (inc generation) new-state current-pattern current-sum offset)))))))

(solve)
