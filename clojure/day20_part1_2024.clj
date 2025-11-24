
(ns race
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:const dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn read-input []
  (let [lines (str/split-lines (slurp "input.txt"))
        h (count lines)
        w (count (first lines))
        grid (vec (for [l lines] (vec l)))
        start (first (for [i (range h) j (range w) :when (= (get-in grid [i j]) \S)] [i j]))
        end (first (for [i (range h) j (range w) :when (= (get-in grid [i j]) \E)] [i j]))
        walls (set (for [i (range h) j (range w) :when (= (get-in grid [i j]) \#)] [i j]))
        track (vec (for [i (range h) j (range w) :when (not (walls [i j]))] [i j]))]
    {:h h :w w :start start :end end :walls walls :track track}))

(defn bfs-dist [start {:keys [h w walls]}]
  (let [dist (java.util.HashMap.)
        q (java.util.ArrayDeque.)]
    (.put dist start 0)
    (.add q [start 0])
    (while (not (.isEmpty q))
      (let [[[i j] d] (.poll q)]
        (doseq [[di dj] dirs]
          (let [ni (+ i di) nj (+ j dj) nxt [ni nj]]
            (when (and (< -1 ni h) (< -1 nj w) (not (walls nxt)) (not (.containsKey dist nxt)))
              (.put dist nxt (inc d))
              (.add q [nxt (inc d)]))))))
    dist))

(defn -main []
  (let [{:keys [h w start end walls track]} (read-input)
        dist-s (bfs-dist start {:h h :w w :walls walls})
        dist-e (bfs-dist end {:h h :w w :walls walls})
        normal (.get dist-s end)
        ans (atom 0)]
    (doseq [[i j] track
            :let [sd (.get dist-s [i j])]
            :when sd]
      (doseq [[di dj] dirs
              :let [m1 [(+ i di) (+ j dj)]]
              :when (and (< -1 (first m1) h) (< -1 (second m1) w))]
        (doseq [[di2 dj2] dirs
                :let [m2 [(+ (first m1) di2) (+ (second m1) dj2)]]
                :when (and (< -1 (first m2) h) (< -1 (second m2) w) (not (walls m2)))]
          (let [ed (.get dist-e m2)
                new-cost (+ sd 2 ed)]
            (when (and ed (>= (- normal new-cost) 100)) (swap! ans inc))))))
  (println @ans)))

(-main)
