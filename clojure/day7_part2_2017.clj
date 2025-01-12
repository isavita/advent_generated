
(require '[clojure.string :as str])

(defn parse-line [line]
  (let [[name weight & holds] (re-seq #"[a-z]+|\d+" line)]
    {:name name
     :weight (Integer/parseInt weight)
     :holds (or holds [])}))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-line)
       (reduce (fn [m {:keys [name] :as prog}] (assoc m name prog)) {})))

(defn dfs [name programs]
  (let [{:keys [weight holds]} (get programs name)
        child-weights (map #(dfs % programs) holds)
        balanced? (every? second child-weights)]
    (if (not balanced?)
      [0 false]
      (let [total-weight (+ weight (reduce + (map first child-weights)))
            weights (frequencies (map first child-weights))
            unique-weights (keys weights)]
        (if (<= (count unique-weights) 1)
          [total-weight true]
          (let [unbalanced-weight (first (filter (fn [[_ count]] (= count 1)) weights))
                balanced-weight (first (filter (fn [[_ count]] (> count 1)) weights))
                unbalanced-child (first (filter (fn [child] (= (first (dfs child programs)) (first unbalanced-weight))) holds))
                diff (- (first balanced-weight) (first unbalanced-weight))]
            (println (+ (:weight (get programs unbalanced-child)) diff))
            [0 false]))))))

(defn solve []
  (let [programs (read-input "input.txt")
        root "dtacyn"]
    (dfs root programs)))

(solve)
