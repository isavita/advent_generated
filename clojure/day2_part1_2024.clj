
(defn parse-line [line]
  (map #(Integer/parseInt %) (clojure.string/split line #" ")))

(defn is-increasing? [levels]
  (every? (fn [[a b]] (< a b)) (partition 2 1 levels)))

(defn is-decreasing? [levels]
  (every? (fn [[a b]] (> a b)) (partition 2 1 levels)))

(defn valid-diff? [levels]
  (every? (fn [[a b]] (let [diff (Math/abs (- a b))] (and (>= diff 1) (<= diff 3)))) (partition 2 1 levels)))

(defn is-safe? [levels]
  (and (or (is-increasing? levels) (is-decreasing? levels))
       (valid-diff? levels)))

(defn solve [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> (line-seq rdr)
         (map parse-line)
         (filter is-safe?)
         count)))

(println (solve "input.txt"))
