
(defn parse-line [line]
  (let [[from _ _ change-str _ _ _ _ _ _ to-str] (clojure.string/split line #" ")
        change (Integer/parseInt change-str)
        to (subs to-str 0 (dec (count to-str)))]
    [from to (if (clojure.string/includes? line "lose") (- change) change)]))

(defn read-happiness-values [filename]
  (->> (slurp filename)
       (clojure.string/split-lines)
       (map parse-line)
       (reduce (fn [m [from to change]]
                 (assoc m from (assoc (get m from {}) to change)))
               {})))

(defn add-yourself [happiness-map]
  (let [guests (keys happiness-map)]
    (reduce (fn [m guest]
              (-> m
                  (assoc-in ["You" guest] 0)
                  (assoc-in [guest "You"] 0)))
            (assoc happiness-map "You" {})
            guests)))

(defn get-guest-list [happiness-map]
  (keys happiness-map))

(defn calculate-happiness [arrangement happiness-map]
  (let [n (count arrangement)]
    (reduce + (for [i (range n)]
                (let [left (mod (dec i) n)
                      right (mod (inc i) n)]
                  (+ (get-in happiness-map [(nth arrangement i) (nth arrangement left)] 0)
                     (get-in happiness-map [(nth arrangement i) (nth arrangement right)] 0)))))))

(defn permutations [coll]
  (if (empty? coll)
    '(())
    (for [x coll
          p (permutations (remove #{x} coll))]
      (cons x p))))

(defn calculate-optimal-arrangement [guests happiness-map]
  (->> (permutations guests)
       (map #(calculate-happiness % happiness-map))
       (apply max 0)))

(defn solve []
  (let [happiness-map (-> "input.txt" read-happiness-values add-yourself)
        guests (get-guest-list happiness-map)]
    (calculate-optimal-arrangement guests happiness-map)))

(println (solve))
