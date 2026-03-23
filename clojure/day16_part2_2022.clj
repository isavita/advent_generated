
(require '[clojure.string :as str])

(defn solve []
  (let [lines (str/split-lines (slurp "input.txt"))
        valves (into {} (for [line lines
                             :let [[_ v f ts] (re-find #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)" line)
                                   tunnels (str/split ts #", ")]]
                         [v {:flow (Integer/parseInt f) :tunnels tunnels}]))
        nodes (keys valves)
        dist (reduce (fn [d u]
                       (reduce (fn [d2 v]
                                 (assoc-in d2 [u v] (cond (= u v) 0
                                                          (some #{v} (get-in valves [u :tunnels])) 1
                                                          :else 1000)))
                               d nodes))
                     {} nodes)
        dist (reduce (fn [d k]
                       (reduce (fn [d2 i]
                                 (reduce (fn [d3 j]
                                           (assoc-in d3 [i j] (min (get-in d3 [i j])
                                                                   (+ (get-in d3 [i k]) (get-in d3 [k j])))))
                                         d2 nodes))
                               d nodes))
                     dist nodes)
        useful (vec (filter #(> (:flow (valves %)) 0) nodes))
        useful-count (count useful)
        results (atom {})]

    ((fn dfs [curr time mask score]
       (swap! results update mask (fnil max 0) score)
       (dotimes [i useful-count]
         (let [v (useful i)
               v-bit (bit-shift-left 1 i)]
           (when (zero? (bit-and mask v-bit))
             (let [ntime (- time (get-in dist [curr v]) 1)]
               (when (> ntime 0)
                 (dfs v ntime (bit-or mask v-bit) (+ score (* ntime (:flow (valves v)))))))))))
     "AA" 26 0 0)

    (let [res-map @results
          masks (keys res-map)]
      (println
       (reduce max 0
               (for [m1 masks
                     m2 masks
                     :when (zero? (bit-and m1 m2))]
                 (+ (res-map m1) (res-map m2))))))))

(solve)

