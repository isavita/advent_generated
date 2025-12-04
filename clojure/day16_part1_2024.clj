
(ns solution
  (:require [clojure.java.io :as io])
  (:import [java.util PriorityQueue]))

(defn -main []
  (let [grid (with-open [r (io/reader "input.txt")]
               (vec (line-seq r)))
        rows (count grid)
        cols (count (first grid))
        start (first (for [i (range rows)
                           j (range cols)
                           :when (= (get-in grid [i j]) \S)]
                       [i j]))
        end   (first (for [i (range rows)
                           j (range cols)
                           :when (= (get-in grid [i j]) \E)]
                       [i j]))]
    (when (or (nil? start) (nil? end))
      (binding [*out* *err*]
        (println "Start or End not found"))
      (System/exit 1))

    (let [start-row (first start)
          start-col (second start)
          end-row   (first end)
          end-col   (second end)
          ;; visited[r][c][dir] as primitive boolean array
          visited   (make-array Boolean/TYPE rows cols 4)
          ;; directions: 0=E, 1=S, 2=W, 3=N
          dirs      [[0 1] [1 0] [0 -1] [-1 0]]
          pq        (PriorityQueue. 11 (comparator #(< (first %1) (first %2))))]
      (.add pq [0 start-row start-col 0])   ; start facing East

      (loop []
        (if (.isEmpty pq)
          (do
            (binding [*out* *err*]
              (println "End not reachable"))
            (System/exit 1))
          (let [state (.poll pq)
                cost (nth state 0)
                r    (nth state 1)
                c    (nth state 2)
                dir  (nth state 3)]
            (if (aget visited r c dir)
              (recur)
              (do
                (aset visited r c dir true)
                (cond
                  (and (= r end-row) (= c end-col))
                  (do (println cost) (System/exit 0))

                  :else
                  (let [[dr dc] (dirs dir)
                        nr (+ r dr)
                        nc (+ c dc)]
                    ;; move forward
                    (when (and (<= 0 nr (dec rows))
                               (<= 0 nc (dec cols))
                               (not= \# (get-in grid [nr nc])))
                      (when-not (aget visited nr nc dir)
                        (.add pq [(inc cost) nr nc dir])))
                    ;; rotate clockwise
                    (let [nd (mod (inc dir) 4)]
                      (when-not (aget visited r c nd)
                        (.add pq [(+ cost 1000) r c nd])))
                    ;; rotate counter-clockwise
                    (let [nd (mod (dec dir) 4)]
                      (when-not (aget visited r c nd)
                        (.add pq [(+ cost 1000) r c nd])))
                    (recur)))))))))))

(-main)
