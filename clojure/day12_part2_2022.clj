
(ns hill-climbing-algorithm
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn read-heightmap [filename]
  (with-open [rdr (io/reader filename)]
    (doall (map vec (line-seq rdr)))))

(defn find-start-and-end [heightmap]
  (let [rows (count heightmap)
        cols (count (first heightmap))]
    (loop [r 0
           start nil
           end nil]
      (if (>= r rows)
        [start end]
        (let [row (nth heightmap r)]
          (loop [c 0]
            (if (>= c cols)
              (recur (inc r) start end)
              (let [char (nth row c)]
                (cond
                  (= char \S) (recur (inc c) [r c] end)
                  (= char \E) (recur (inc c) start [r c])
                  :else (recur (inc c) start end))))))))))

(defn neighbors [heightmap [r c]]
  (let [directions [[1 0] [-1 0] [0 1] [0 -1]]
        rows (count heightmap)
        cols (count (first heightmap))]
    (for [[dr dc] directions
          :let [nr (+ r dr)
                nc (+ c dc)]
          :when (and (>= nr 0) (< nr rows) (>= nc 0) (< nc cols))]
      [nr nc])))

(defn valid-move? [heightmap from to]
  (let [from-elevation (if (= (nth (nth heightmap (first from)) (second from)) \S) \a
                          (nth (nth heightmap (first from)) (second from)))
        to-elevation (if (= (nth (nth heightmap (first to)) (second to)) \E) \z
                        (nth (nth heightmap (first to)) (second to)))]
    (or (<= to-elevation from-elevation)
        (= to-elevation (inc from-elevation)))))

(defn bfs [heightmap start end]
  (let [queue (atom [[start 0]]) ; [position steps]
        visited (atom #{})]
    (loop []
      (when (seq @queue)
        (let [[[current pos] steps] (first @queue)]
          (swap! queue rest)
          (if (= current end)
            steps
            (do
              (swap! visited conj current)
              (doseq [neighbor (neighbors heightmap current)
                      :when (and (not (@visited neighbor))
                                 (valid-move? heightmap current neighbor))]
                (swap! queue conj [neighbor (inc steps)]))
              (recur))))))))

(defn find-shortest-path [heightmap]
  (let [[start end] (find-start-and-end heightmap)
        end-elevation (if (= (nth (nth heightmap (first end)) (second end)) \E) \z
                         (nth (nth heightmap (first end)) (second end)))
        a-positions (for [r (range (count heightmap))
                          c (range (count (first heightmap)))
                          :when (or (= (nth (nth heightmap r) c) \S)
                                    (= (nth (nth heightmap r) c) \a))]
                      [r c])]
    (apply min (map #(bfs heightmap % end) a-positions))))

(defn -main []
  (let [heightmap (read-heightmap "input.txt")
        steps (find-shortest-path heightmap)]
    (println "Fewest steps required:" steps)))

(-main)
