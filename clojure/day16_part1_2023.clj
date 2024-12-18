
(defn parse-grid [lines]
  (let [width (count (first lines))
        height (count lines)
        data (reduce (fn [m [y line]]
                       (reduce (fn [m [x char]]
                                 (if (not= char \.)
                                   (assoc m [x y] char)
                                   m))
                               m
                               (map-indexed vector line)))
                     {}
                     (map-indexed vector lines))]
    {:width width :height height :data data}))

(def north [0 -1])
(def west [-1 0])
(def south [0 1])
(def east [1 0])

(defn add-coord [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn rotate-90 [[x y]]
  [y (- x)])

(defn rotate-neg-90 [[x y]]
  [(- y) x])

(defn in-bounds? [[x y] {:keys [width height]}]
  (and (>= x 0) (< x width) (>= y 0) (< y height)))

(defn next-beam [{:keys [data] :as grid} [[x y] dir]]
  (let [char (get data [x y])]
    (cond
      (nil? char) [[(add-coord [x y] dir) dir]]
      (= char \/) (let [new-dir (if (or (= dir north) (= dir south)) (rotate-neg-90 dir) (rotate-90 dir))]
                    [[(add-coord [x y] new-dir) new-dir]])
      (= char \\) (let [new-dir (if (or (= dir north) (= dir south)) (rotate-90 dir) (rotate-neg-90 dir))]
                    [[(add-coord [x y] new-dir) new-dir]])
      (and (= char \|) (or (= dir east) (= dir west)))
      (let [new-dir1 (rotate-90 dir)
            new-dir2 (rotate-neg-90 dir)]
        [[(add-coord [x y] new-dir1) new-dir1] [(add-coord [x y] new-dir2) new-dir2]])
      (and (= char \-) (or (= dir north) (= dir south)))
      (let [new-dir1 (rotate-90 dir)
            new-dir2 (rotate-neg-90 dir)]
        [[(add-coord [x y] new-dir1) new-dir1] [(add-coord [x y] new-dir2) new-dir2]])
      :else [[(add-coord [x y] dir) dir]])))

(defn calculate-propagation [grid start]
  (loop [seen #{}
         to-explore [start]]
    (if (empty? to-explore)
      seen
      (let [[[x y] dir :as beam] (first to-explore)]
        (if (and (in-bounds? [x y] grid) (not (contains? seen beam)))
          (recur (conj seen beam) (concat (rest to-explore) (next-beam grid beam)))
          (recur seen (rest to-explore)))))))

(defn calculate-energization [seen]
  (reduce (fn [s [[x y] _]] (conj s [x y])) #{} seen))

(defn solve [lines]
  (let [grid (parse-grid lines)
        start [[0 0] east]
        seen (calculate-propagation grid start)
        energized (calculate-energization seen)]
    (count energized)))

(defn read-file [filename]
  (-> filename slurp (clojure.string/trim) (clojure.string/split #"\n")))

(println (solve (read-file "input.txt")))
