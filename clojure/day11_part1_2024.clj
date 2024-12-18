
(defn apply-rules [stone]
  (let [s (str stone)]
    (cond
      (= stone 0) [1]
      (even? (count s)) [(Integer/parseInt (subs s 0 (/ (count s) 2)))
                         (Integer/parseInt (subs s (/ (count s) 2)))]
      :else [( * stone 2024)])))

(defn blink [stones]
  (->> stones
       (mapcat apply-rules)
       (vec)))

(defn solve [initial-stones num-blinks]
  (loop [stones initial-stones
         blinks 0]
    (if (= blinks num-blinks)
      stones
      (recur (blink stones) (inc blinks)))))

(defn parse-input [input-str]
  (->> (clojure.string/split input-str #"\s+")
       (map #(Integer/parseInt %))
       (vec)))

(defn -main []
  (let [input-str (slurp "input.txt")
        initial-stones (parse-input input-str)
        final-stones (solve initial-stones 25)]
    (println (count final-stones))))

(-main)
