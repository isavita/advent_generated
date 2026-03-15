
(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-input [filename]
  (with-open [rdr (io/reader filename)]
    (reduce
      (fn [graph line]
        (let [[node neighbors] (str/split line #": ")
              neighbors (str/split neighbors #" ")]
          (reduce
            (fn [g neighbor]
              (-> g
                  (update node (fnil conj #{}) neighbor)
                  (update neighbor (fnil conj #{}) node)))
            graph
            neighbors)))
      {}
      (line-seq rdr))))

(defn bfs [graph start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         visited {start start}]
    (if (empty? queue)
      visited
      (let [current (peek queue)
            queue (pop queue)
            neighbors (remove visited (get graph current #{}))]
        (recur
          (into queue neighbors)
          (into visited (map #(vector % current) neighbors)))))))

(defn find-path [came-from start end]
  (loop [path [end]
         current end]
    (if (= current start)
      (reverse path)
      (recur (conj path (came-from current)) (came-from current)))))

(defn remove-path [graph path]
  (reduce
    (fn [g [a b]]
      (-> g
          (update a disj b)
          (update b disj a)))
    graph
    (partition 2 1 path)))

(defn solve []
  (let [graph (parse-input "input.txt")
        source (first (keys graph))
        min-cut 3]
    (loop [ends (disj (set (keys graph)) source)]
      (if (empty? ends)
        (throw (Exception. "No solution found"))
        (let [end (first ends)
              new-graph (reduce
                          (fn [g _]
                            (let [came-from (bfs g source)]
                              (if-not (contains? came-from end)
                                (reduced nil)
                                (remove-path g (find-path came-from source end)))))
                          graph
                          (range min-cut))]
          (if (and new-graph (not (contains? (bfs new-graph source) end)))
            (let [partition1 (count (bfs new-graph source))
                  partition2 (- (count graph) partition1)]
              (* partition1 partition2))
            (recur (disj ends end))))))))

(println (solve))
