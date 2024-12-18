
(defn parse-levels [line]
  (mapv #(Integer/parseInt %) (clojure.string/split line #" ")))

(defn safe-report? [levels]
  (if (< (count levels) 2)
    false
    (let [first-diff (- (nth levels 1) (first levels))]
      (if (zero? first-diff)
        false
        (let [is-increasing? (> first-diff 0)]
          (loop [i 0]
            (if (>= i (dec (count levels)))
              true
              (let [diff (- (nth levels (inc i)) (nth levels i))
                    abs-diff (Math/abs (double diff))]
                (if (or (zero? diff)
                        (and is-increasing? (<= diff 0))
                        (and (not is-increasing?) (>= diff 0))
                        (or (< abs-diff 1) (> abs-diff 3)))
                  false
                  (recur (inc i)))))))))))

(defn safe-with-one-removal? [levels]
  (some (fn [i]
          (let [modified-levels (vec (concat (subvec levels 0 i) (subvec levels (inc i))))]
            (safe-report? modified-levels)))
        (range (count levels))))

(defn solve []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (->> (line-seq rdr)
         (map parse-levels)
         (filter #(or (safe-report? %) (safe-with-one-removal? %)))
         count)))

(println (solve))
