
(defn parse-line [line]
  (map #(Integer/parseInt %) (clojure.string/split line #" ")))

(defn differences [seq]
  (map - (rest seq) seq))

(defn all-zeros? [seq]
  (every? zero? seq))

(defn extrapolate-next [history]
  (loop [seqs [history]]
    (let [current-seq (last seqs)
          diffs (differences current-seq)]
      (if (all-zeros? diffs)
        (reduce + (map last seqs))
        (recur (conj seqs diffs))))))

(defn solve [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> (line-seq rdr)
         (map parse-line)
         (map extrapolate-next)
         (reduce +))))

(println (solve "input.txt"))
