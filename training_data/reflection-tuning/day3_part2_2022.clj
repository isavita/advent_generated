(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn char-priority [c]
  (if (Character/isUpperCase c)
    (- (int c) 38)
    (- (int c) 96)))

(defn find-common-item [compartments]
  (first (apply set/intersection (map set compartments))))

(defn solve-part1 [lines]
  (->> lines
       (map #(split-at (/ (count %) 2) %))
       (map find-common-item)
       (map char-priority)
       (reduce +)))

(defn solve-part2 [lines]
  (->> lines
       (partition 3)
       (map find-common-item)
       (map char-priority)
       (reduce +)))

(let [lines (str/split-lines (slurp "input.txt"))]
  (println "Part 1:" (solve-part1 lines))
  (println "Part 2:" (solve-part2 lines)))
