(require '[clojure.string :as str])

(defn parse-input [input]
  (let [[seeds & maps] (str/split input #"\n\n")
        seed-ranges (->> (re-seq #"\d+" seeds)
                         (map parse-long)
                         (partition 2)
                         (map (fn [[start len]] [start (+ start len -1)])))
        parse-map (fn [m]
                    (->> (str/split-lines m)
                         (rest)
                         (map #(map parse-long (re-seq #"\d+" %)))
                         (map (fn [[dest src len]] {:src src :dest dest :len len}))
                         (sort-by :src)))]
    {:seed-ranges seed-ranges
     :maps (map parse-map maps)}))

(defn apply-map-to-range [[start end] m]
  (loop [ranges [[start end]]
         result []
         [{:keys [src dest len]} & rest-m] m]
    (if (nil? src)
      (into result ranges)
      (let [src-end (+ src len -1)
            [before overlap after] (partition-by #(cond (< % src) :before
                                                       (<= % src-end) :overlap
                                                       :else :after)
                                                 (range start (inc end)))
            new-ranges (cond-> []
                         (seq before) (conj [(first before) (last before)])
                         (seq overlap) (conj [(+ dest (- (first overlap) src))
                                              (+ dest (- (last overlap) src))])
                         (seq after) (conj [(first after) (last after)]))]
        (recur (filter #(<= src-end (first %)) new-ranges)
               (into result (filter #(> src (second %)) new-ranges))
               rest-m)))))

(defn process-range [maps range]
  (reduce (fn [r m] (mapcat #(apply-map-to-range % m) r))
          [range]
          maps))

(defn solve [input]
  (let [{:keys [seed-ranges maps]} (parse-input input)]
    (->> seed-ranges
         (mapcat #(process-range maps %))
         (map first)
         (apply min))))

(def input (slurp "input.txt"))
(println (solve input))
