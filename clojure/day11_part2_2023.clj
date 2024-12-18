
(defn parse-grid [lines empty]
  (let [width (count (first lines))
        height (count lines)]
    (reduce (fn [grid [y line]]
              (reduce (fn [g [x char]]
                        (if (not= char empty)
                          (assoc g [x y] char)
                          g))
                      grid
                      (map-indexed vector line)))
            {}
            (map-indexed vector lines))))

(defn get-empty-rows [grid width height]
  (filter (fn [y]
            (every? (fn [x] (not (contains? grid [x y]))) (range width)))
          (range height)))

(defn get-empty-cols [grid width height]
  (filter (fn [x]
            (every? (fn [y] (not (contains? grid [x y]))) (range height)))
          (range width)))

(defn calculate-offsets [empty-indexes bound]
  (let [offsets (vec (repeat bound 0))]
    (reduce (fn [acc idx]
              (loop [i (inc idx)
                     a acc]
                (if (< i bound)
                  (recur (inc i) (assoc a i (inc (a i))))
                  a)))
            offsets
            empty-indexes)))

(defn expand-grid [grid width height expansion-factor]
  (let [empty-cols (get-empty-cols grid width height)
        empty-rows (get-empty-rows grid width height)
        num-lines-to-add (dec expansion-factor)
        dxs (calculate-offsets empty-cols width)
        dys (calculate-offsets empty-rows height)
        new-width (+ width (* (count empty-cols) num-lines-to-add))
        new-height (+ height (* (count empty-rows) num-lines-to-add))]
    (reduce (fn [new-grid [[x y] char]]
              (let [new-x (+ x (* (dxs x) num-lines-to-add))
                    new-y (+ y (* (dys y) num-lines-to-add))]
                (assoc new-grid [new-x new-y] char)))
            {}
            grid)))

(defn abs [x] (if (< x 0) (- x) x))

(defn calculate-length [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn solve [lines expansion-factor]
  (let [empty \.
        grid (parse-grid lines empty)
        width (count (first lines))
        height (count lines)
        expanded-grid (expand-grid grid width height expansion-factor)
        coords (keys expanded-grid)]
    (loop [remaining coords
           seen #{}
           total 0]
      (if (empty? remaining)
        total
        (let [current (first remaining)
              new-total (reduce (fn [acc c] (+ acc (calculate-length current c)))
                              total
                              seen)]
          (recur (rest remaining)
                 (conj seen current)
                 new-total))))))

(defn read-file [filename]
  (-> filename
      slurp
      (clojure.string/trim)
      (clojure.string/split #"\n")))

(defn -main []
  (let [input (read-file "input.txt")]
    (println (solve input 1000000))))

(-main)
