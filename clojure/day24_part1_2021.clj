
(require '[clojure.string :as str])

(defn parse-input []
  (let [lines (-> (slurp "input.txt") str/trim (str/split #"\n"))
        extract-nums (fn [indices]
                       (mapv #(let [parts (str/split (nth lines %) #"\s")]
                               (Integer/parseInt (nth parts 2)))
                             indices))]
    {:l (extract-nums (range 4 (count lines) 18))
     :k (extract-nums (range 5 (count lines) 18))
     :m (extract-nums (range 15 (count lines) 18))}))

(defn build-constraints [{:keys [l k m]}]
  (loop [i 0 stack [] constraints {}]
    (if (>= i (count l))
      constraints
      (let [li (nth l i)]
        (cond
          (= li 1) (recur (inc i) (conj stack i) constraints)
          (= li 26) (let [pop-idx (peek stack)
                         offset (+ (nth m pop-idx) (nth k i))]
                     (recur (inc i) (pop stack)
                            (assoc constraints pop-idx [i offset])))
          :else (recur (inc i) stack constraints))))))

(defn solve []
  (let [{:keys [l k m]} (parse-input)
        constraints (build-constraints {:l l :k k :m m})
        digits (vec (repeat 14 0))]
    (reduce-kv
      (fn [acc pop-idx [other-idx offset]]
        (let [vmax (loop [v 9]
                    (if (<= (+ v offset) 9) v (recur (dec v))))]
          (-> acc
              (assoc pop-idx vmax)
              (assoc other-idx (+ vmax offset)))))
      digits
      constraints)))

(println (Long/parseLong (apply str (solve))))
