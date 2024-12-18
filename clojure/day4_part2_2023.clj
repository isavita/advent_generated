
(require '[clojure.string :as str])

(defn parse-card [line]
  (let [[_ winning-str have-str] (re-matches #"Card\s+\d+:\s+([\d\s]+)\|\s+([\d\s]+)" line)
        winning-nums (-> winning-str (str/trim) (str/split #"\s+") (->> (map parse-long) (set)))
        have-nums (-> have-str (str/trim) (str/split #"\s+") (->> (map parse-long)))]
    {:winning winning-nums :have have-nums}))

(defn count-matches [{:keys [winning have]}]
  (count (filter winning have)))

(defn calculate-points [matches]
  (if (zero? matches)
    0
    (bit-shift-left 1 (dec matches))))

(defn solve-part1 [lines]
  (->> lines
       (map parse-card)
       (map count-matches)
       (map calculate-points)
       (reduce +)))

(defn solve-part2 [lines]
  (let [cards (map parse-card lines)
        card-count (count cards)
        matches (map count-matches cards)
        initial-counts (vec (repeat card-count 1))]
    (loop [card-idx 0
           counts initial-counts]
      (if (>= card-idx card-count)
        (reduce + counts)
        (let [current-count (nth counts card-idx)
              num-matches (nth matches card-idx)]
          (if (zero? num-matches)
            (recur (inc card-idx) counts)
            (let [updated-counts (reduce (fn [acc i]
                                           (if (< i card-count)
                                             (update acc i + current-count)
                                             acc))
                                         counts
                                         (range (inc card-idx) (+ (inc card-idx) num-matches)))]
              (recur (inc card-idx) updated-counts))))))))

(defn -main []
  (let [lines (-> "input.txt" slurp str/split-lines)]
    (println "Part 1:" (solve-part1 lines))
    (println "Part 2:" (solve-part2 lines))))

(-main)
