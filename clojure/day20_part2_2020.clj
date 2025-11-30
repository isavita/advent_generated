
(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(defn parse-tiles [input]
  (for [block (str/split input #"\n\n")]
    (let [[header & lines] (str/split-lines block)
          id (Long/parseLong (subs header 5 (dec (count header))))]
      {:id id :grid (mapv vec lines)})))

(defn rot90 [grid]
  (let [rows (count grid) cols (count (first grid))]
    (vec (for [j (range cols)]
           (vec (for [i (range rows)]
                  (get-in grid [rows i (- cols 1 j)]))))))

(defn mirror [grid] (mapv rseq grid))

(defn orientations [grid]
  (let [rots (take 4 (iterate rot90 grid))]
    (concat rots (map mirror rots))))

(defn top [grid] (first grid))
(defn bottom [grid] (last grid))
(defn left [grid] (mapv first grid))
(defn right [grid] (mapv last grid))

(defn remove-borders [grid]
  (mapv (fn [row] (subvec row 1 (dec (count row))))
        (subvec grid 1 (dec (count grid)))))

(defn assemble [tiles edge]
  (let [n (count tiles)
        used (atom #{})
        board (atom (vec (repeat edge (vec (repeat edge nil)))))]
    (letfn [(place [row col]
              (cond
                (== row edge) true
                (get-in @board [row col]) (recur (if (== col (dec edge)) (inc row) row)
                                                 (if (== col (dec edge)) 0 (inc col)))
                :else
                (loop [ids (range n)]
                  (if (empty? ids)
                    false
                      (let [i (first ids)]
                        (if (contains? @used i)
                          (recur (rest ids))
                          (let [tile (nth tiles i)]
                            (loop [os (orientations (:grid tile))]
                              (if (empty? os)
                                (recur (rest ids))
                                (let [g (first os)
                                      fits (and (or (zero? row) (= (top g) (bottom (get-in @board [(dec row) col]))))
                                              (or (zero? col) (= (left g) (right (get-in @board [row (dec col)]))))]
                                  (if fits
                                    (do
                                      (swap! used conj i)
                                      (swap! board assoc-in [row col] (assoc tile :grid g))
                                      (if (place (if (== col (dec edge)) (inc row) row)
                                                 (if (== col (dec edge)) 0 (inc col)))
                                        true
                                        (do
                                          (swap! used disj i)
                                          (swap! board assoc-in [row col] nil)
                                          (recur (rest os)))))
                                    (recur (rest os))))))))))))]
      (when (place 0 0) @board))))

(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(defn monster-cells []
  (for [[r line] (map-indexed vector monster)
        [c ch] (map-indexed vector line)
        :when (= ch \#)]
    [r c]))

(defn find-monsters [grid]
  (let [mh (count monster) mw (count (first monster))
        cells (monster-cells)
        h (count grid) w (count (first grid))]
    (for [r (range (inc (- h mh)))
          c (range (inc (- w mw)))
          :when (every? (fn [[dr dc]] (= (get-in grid [(+ r dr) (+ c dc)]) \#)))
                         cells]
      [r c])))

(defn mark-monsters [grid]
  (let [cells (monster-cells)
        monsters (find-monsters grid)]
    (reduce (fn [g [mr mc]]
              (reduce (fn [acc [dr dc]]
                        (assoc-in acc [(+ mr dr) (+ mc dc)] \O))
                      g cells))
            grid monsters)))

(defn -main [& _]
  (let [tiles (parse-tiles (slurp "input.txt"))
        edge (long (Math/sqrt (count tiles)))
        board (assemble tiles edge)
        strip (mapv (partial mapv remove-borders) board)
        rows (mapv (partial apply concat) (apply map concat strip))
        [final] (drop-while empty? (map (fn [g] [(mark-monsters g) (find-monsters g)])
                                         (orientations rows)))
        rough (count (filter #(= \# %) (apply concat (first final)))]
    (println rough)))

(-main)
