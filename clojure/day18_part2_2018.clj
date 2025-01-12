
(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (map vec (line-seq rdr)))))

(defn count-adjacent [grid i j acre-type]
  (let [rows (count grid)
        cols (count (first grid))]
    (count (for [x (range -1 2)
                 y (range -1 2)
                 :when (not= 0 x y)
                 :let [ni (+ i x)
                       nj (+ j y)]
                 :when (and (>= ni 0) (< ni rows) (>= nj 0) (< nj cols) (= (get-in grid [ni nj]) acre-type))]
             true))))

(defn next-acre-state [grid i j]
  (case (get-in grid [i j])
    \. (if (>= (count-adjacent grid i j \|) 3) \| \.)
    \| (if (>= (count-adjacent grid i j \#) 3) \# \|)
    \# (if (and (>= (count-adjacent grid i j \#) 1) (>= (count-adjacent grid i j \|) 1)) \# \.)
    (get-in grid [i j])))

(defn transform [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (vec (for [i (range rows)]
           (vec (for [j (range cols)]
                  (next-acre-state grid i j)))))))

(defn count-resources [grid]
  (let [wooded (atom 0)
        lumberyards (atom 0)]
    (doseq [row grid
            acre row]
      (case acre
        \| (swap! wooded inc)
        \# (swap! lumberyards inc)
        nil))
    [@wooded @lumberyards]))

(defn grid-to-string [grid]
  (apply str (interpose "\n" (map (partial apply str) grid))))

(defn solve [filename]
  (let [grid (read-input filename)
        seen-states (atom {})
        cycle-start (atom 0)
        cycle-length (atom 0)]
    (loop [minute 0
           current-grid grid]
      (let [state (grid-to-string current-grid)]
        (if-let [seen-minute (@seen-states state)]
          (do
            (reset! cycle-start seen-minute)
            (reset! cycle-length (- minute seen-minute))
            (let [remaining-minutes (mod (- 1000000000 @cycle-start) @cycle-length)
                  final-grid (nth (iterate transform grid) (+ @cycle-start remaining-minutes))]
              (let [[wooded lumberyards] (count-resources final-grid)]
                (* wooded lumberyards))))
          (do
            (swap! seen-states assoc state minute)
            (recur (inc minute) (transform current-grid))))))))

(println (solve "input.txt"))
