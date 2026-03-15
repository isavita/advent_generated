
(require '[clojure.string :as str])

(def directions [[0 -1] [0 1] [-1 0] [1 0]])

(defn parse-input [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        grid (vec (map vec lines))]
    {:width width :height height :grid grid}))

(defn in-bounds? [{:keys [width height]} [x y]]
  (and (<= 0 x (dec width)) (<= 0 y (dec height))))

(defn get-cell [{:keys [grid]} [x y]]
  (get-in grid [y x]))

(defn empty-cell? [grid coord]
  (let [cell (get-cell grid coord)]
    (and cell (not= cell \#))))

(defn neighbors4 [grid coord]
  (let [[x y] coord]
    (for [[dx dy] directions
          :let [nx (+ x dx) ny (+ y dy)
                neighbor [nx ny]]
          :when (and (in-bounds? grid neighbor)
                    (empty-cell? grid neighbor))]
      neighbor)))

(defn find-vertices [grid start end]
  (let [vertices (atom #{start end})]
    (doseq [y (range (:height grid))
            x (range (:width grid))
            :let [coord [x y]]
            :when (and (empty-cell? grid coord)
                      (> (count (neighbors4 grid coord)) 2))]
      (swap! vertices conj coord))
    @vertices))

(defn bfs-edges [grid start vertices]
  (let [queue (atom (conj clojure.lang.PersistentQueue/EMPTY [start 0]))
        visited (atom #{start})
        edges (atom {})]
    (while (seq @queue)
      (let [[current dist] (peek @queue)
            _ (swap! queue pop)]
        (if (and (not= current start) (contains? vertices current))
          (swap! edges assoc current dist)
          (doseq [next (neighbors4 grid current)
                  :when (not (contains? @visited next))]
            (swap! visited conj next)
            (swap! queue conj [next (inc dist)])))))
    @edges))

(defn build-graph [grid start end]
  (let [vertices (find-vertices grid start end)
        edges (atom {})]
    (doseq [vertex vertices]
      (swap! edges assoc vertex (bfs-edges grid vertex vertices)))
    {:vertices vertices :edges @edges}))

(defn dfs-longest [graph current end visited]
  (if (= current end)
    0
    (let [_ (swap! visited conj current)
          current-edges (get-in graph [:edges current] {})
          max-dist (->> current-edges
                        (filter (fn [[next _]] (not (contains? @visited next))))
                        (map (fn [[next weight]]
                               (let [dist (dfs-longest graph next end visited)]
                                 (when (>= dist 0)
                                   (+ weight dist)))))
                        (filter some?)
                        (reduce max -1))]
      (swap! visited disj current)
      max-dist)))

(defn solve [input]
  (let [{:keys [width height grid] :as grid-data} (parse-input input)
        start [1 0]
        end [(- width 2) (dec height)]
        graph (build-graph grid-data start end)
        visited (atom #{})]
    (dfs-longest graph start end visited)))

(let [input (slurp "input.txt")]
  (println (solve input)))
