
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-coords [line]
  (let [[x y] (str/split line #",")]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-coords)))

(defn create-grid [size]
  (vec (repeat size (vec (repeat size false)))))

(defn mark-corrupted [grid [x y]]
  (if (and (>= x 0) (< x (count (first grid)))
           (>= y 0) (< y (count grid)))
    (assoc-in grid [y x] true)
    grid))

(defn get-neighbors [[x y] size]
  (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
       (filter (fn [[nx ny]]
                 (and (>= nx 0) (< nx size)
                      (>= ny 0) (< ny size))))))

(defn bfs [grid start end]
  (let [size (count grid)
        visited (atom #{start})
        queue (clojure.lang.PersistentQueue/EMPTY)
        queue (conj queue start)]
    (loop [q queue]
      (if (empty? q)
        nil
        (let [curr (peek q)]
          (if (= curr end)
            true
            (let [neighbors (->> (get-neighbors curr size)
                                 (remove @visited)
                                 (remove (fn [[x y]] (get-in grid [y x]))))]
              (swap! visited set/union (set neighbors))
              (recur (reduce conj (pop q) neighbors)))))))))

(defn solve-part1 [coords]
  (let [size 71
        grid (create-grid size)
        corrupted-grid (reduce mark-corrupted grid (take 1024 coords))]
    (if (bfs corrupted-grid [0 0] [70 70])
      (throw (Exception. "Path should exist"))
      (throw (Exception. "Path should not exist")))))

(defn solve-part2 [coords]
  (let [size 71
        grid (create-grid size)]
    (loop [idx 0
           current-grid grid]
      (if (>= idx (count coords))
        (throw (Exception. "No blocking byte found"))
        (let [next-grid (mark-corrupted current-grid (nth coords idx))]
          (if (not (bfs next-grid [0 0] [70 70]))
            (nth coords idx)
            (recur (inc idx) next-grid)))))))

(defn -main [& args]
  (let [coords (read-input "input.txt")]
    (println "Part 1:")
    (try
      (solve-part1 coords)
      (catch Exception e (println (.getMessage e))))
    (println "Part 2:" (str/join "," (solve-part2 coords)))))

(-main)
