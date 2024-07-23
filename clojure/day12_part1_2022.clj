
(ns pathfinding.core
  (:require [clojure.java.io :as io]
            [clojure.core.priority-map :as pm]))

(defn read-input []
  (with-open [rdr (io/reader "input.txt")]
    (let [lines (line-seq rdr)]
      (reduce (fn [acc line]
                (reduce (fn [m [x b]]
                          (let [p [x (count acc)]]
                            (cond
                              (= b \S) (assoc m :start p)
                              (= b \E) (assoc m :end p)
                              (= b \a) (update m :as conj p)
                              :else (assoc m p b))))
                        acc
                        (map-indexed vector line)))
              {:grid {} :as []}
              lines))))

(defn neighbors [[x y]]
  [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]])

(defn valid-move? [grid curr next]
  (let [curr-val (get grid curr)
        next-val (get grid next)]
    (and next-val (<= (- (int next-val) (int curr-val)) 1))))

(defn dijkstra [grid start end]
  (let [pq (pm/priority-map end 0)
        dist {end 0}]
    (loop []
      (if (empty? pq)
        dist
        (let [[curr _] (first pq)]
          (let [pq (pm/dissoc pq curr)]
            (doseq [n (neighbors curr)]
              (when (valid-move? grid curr n)
                (let [next-dist (inc (get dist curr 1e9))]
                  (when (< next-dist (get dist n 1e9))
                    (def pq (pm/assoc pq n next-dist))
                    (def dist (assoc dist n next-dist))))))
            (recur)))))))

(defn -main []
  (let [{:keys [grid start end as]} (read-input)]
    (let [grid (-> grid
                   (assoc start \a)
                   (assoc end \z))
          dists (dijkstra grid start)]
      (println (get dists start)))))

(-main)
