(require '[clojure.string :as str])
(import '(java.util PriorityQueue))

(defn parse-line [line]
  (let [[_ dep step] (re-find #"Step (\w) must be finished before step (\w) can begin." line)]
    [dep step]))

(defn build-graph [edges]
  (reduce (fn [[graph in-degree] [dep step]]
            [(update graph dep (fnil conj #{}) step)
             (update in-degree step (fnil inc 0))])
          [{} {}]
          edges))

(defn topological-sort [graph in-degree]
  (loop [queue (PriorityQueue.)
         in-degree in-degree
         result []]
    (if (.isEmpty queue)
      (if (empty? in-degree)
        (str/join result)
        (let [next-steps (filter #(zero? (in-degree %)) (keys in-degree))]
          (doseq [step next-steps]
            (.offer queue step))
          (recur queue in-degree result)))
      (let [current (.poll queue)]
        (recur queue
               (reduce #(update %1 %2 dec) (dissoc in-degree current) (graph current))
               (conj result current))))))

(defn solve-puzzle []
  (let [input (slurp "input.txt")
        edges (map parse-line (str/split-lines input))
        [graph in-degree] (build-graph edges)
        all-steps (set (concat (keys graph) (keys in-degree)))
        initial-steps (filter #(not (contains? in-degree %)) all-steps)
        queue (PriorityQueue.)]
    (doseq [step initial-steps]
      (.offer queue step))
    (topological-sort graph in-degree)))

(println (solve-puzzle))
