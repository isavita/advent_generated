
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-grid [txt]
  (mapv (fn [line] (mapv #(Long/parseLong (str %)) line))
        (str/split-lines txt)))

(defn dijkstra [grid]
  (let [rows (count grid)
        cols (count (first grid))
        goal [(dec rows) (dec cols)]
        init {[0 0] 0}
        dirs [[1 0] [0 1] [-1 0] [0 -1]]
        cmp (fn [a b] (compare (second a) (second b)))
        pq (java.util.PriorityQueue. (fn [a b] (cmp a b)))]
    (.add pq [[0 0] 0])
    (loop [seen #{}
           pq pq]
      (if (.isEmpty pq)
        -1
        (let [[[x y] risk] (.poll pq)]
          (cond
            (= [x y] goal) risk
            (seen [x y]) (recur seen pq)
            :else
            (let [seen' (conj seen [x y])
                  nexts (for [[dx dy] dirs
                              :let [nx (+ x dx)
                                    ny (+ y dy)]
                              :when (and (< -1 nx rows)
                                         (< -1 ny cols)
                                         (not (seen' [nx ny])))]
                          [[nx ny] (+ risk (get-in grid [nx ny]))])]
              (doseq [[pos new-risk] nexts]
                (.add pq [pos new-risk]))
              (recur seen' pq))))))))

(defn -main [& _]
  (let [txt (slurp "input.txt")]
    (println (dijkstra (parse-grid txt)))))

(-main)
