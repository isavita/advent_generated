
(ns day17
  (:require [clojure.string :as str]))

(defn parse-grid [input]
  (->> (str/split-lines input)
       (mapv #(mapv (comp read-string str) %))))

(defn solve [grid]
  (let [rows (count grid)
        cols (count (first grid))
        start [0 0]
        end [(- rows 1) (- cols 1)]
        initial-state {:pos start :dir nil :straight-count 0 :heat-loss 0}
        queue (java.util.PriorityQueue. 100 #(< (:heat-loss %1) (:heat-loss %2)))
        visited (atom #{})]

    (.add queue initial-state)

    (loop []
      (when-not (.isEmpty queue)
        (let [current (.poll queue)
              {:keys [pos dir straight-count heat-loss]} current
              [row col] pos]

          (if (= pos end)
            (reduced heat-loss)
            (if (contains? @visited [pos dir straight-count])
              (recur)
              (do
                (swap! visited conj [pos dir straight-count])
                (let [possible-moves (cond
                                       (nil? dir) [[0 1] [1 0]] ; Initial moves: right or down
                                       (= dir [0 1])  [[0 1] [1 0] [-1 0]] ; Right: continue, down, or up
                                       (= dir [1 0])  [[1 0] [0 1] [0 -1]] ; Down: continue, right, or left
                                       (= dir [0 -1]) [[0 -1] [1 0] [-1 0]] ; Left: continue, down, or up
                                       (= dir [-1 0]) [[-1 0] [0 1] [0 -1]] ; Up: continue, right, or left
                                       )
                      valid-moves (filter (fn [[dr dc]]
                                            (let [new-row (+ row dr)
                                                  new-col (+ col dc)]
                                              (and (>= new-row 0) (< new-row rows)
                                                   (>= new-col 0) (< new-col cols))))
                                          possible-moves)]
                  (doseq [new-dir valid-moves]
                    (let [new-straight-count (if (= new-dir dir) (inc straight-count) 1)
                          [new-row new-col] (map + pos new-dir)]
                      (when (<= new-straight-count 3)
                        (let [new-heat-loss (+ heat-loss (get-in grid [new-row new-col]))
                              new-state {:pos [new-row new-col] :dir new-dir :straight-count new-straight-count :heat-loss new-heat-loss}]
                          (.add queue new-state))))))
                (recur)))))))))

(let [input (slurp "input.txt")
      grid (parse-grid input)]
  (println (solve grid)))
