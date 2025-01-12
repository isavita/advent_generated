
(defn valid-password? [password]
  (let [s (str password)]
    (loop [i 0 has-double false]
      (if (>= i (dec (count s)))
        has-double
        (if (> (int (nth s i)) (int (nth s (inc i))))
          false
          (let [is-double (= (nth s i) (nth s (inc i)))
                not-larger-group (and (or (= i 0) (not= (nth s i) (nth s (dec i))))
                                     (or (>= (+ i 2) (count s)) (not= (nth s i) (nth s (+ i 2)))))]
            (recur (inc i) (or has-double (and is-double not-larger-group)))))))))

(defn solve []
  (let [[start end] (-> (slurp "input.txt")
                        (clojure.string/trim)
                        (clojure.string/split #"-")
                        (->> (map #(Integer/parseInt %))))]
    (->> (range start (inc end))
         (filter valid-password?)
         count)))

(println (solve))
