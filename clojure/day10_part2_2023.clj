
(ns solution
  (:require [clojure.java.io :as io]))

(def directions {:top [0 -1] :right [1 0] :bottom [0 1] :left [-1 0]})

(def tile-to-pipe
  {\| #{:top :bottom}
   \- #{:left :right}
   \J #{:top :left}
   \L #{:top :right}
   \7 #{:bottom :left}
   \F #{:bottom :right}})

(defn opposite [dir] 
  (case dir :top :bottom :bottom :top :left :right :right :left))

(defn add-coord [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn parse-input [lines]
  (let [data (into {} (for [y (range (count lines))
                            x (range (count (first lines)))
                            :let [tile (get-in lines [y x])]
                            :when (not= tile \.)]
                        [[x y] tile]))]
    {:width (count (first lines))
     :height (count lines)
     :data data}))

(defn find-start [grid]
  (ffirst (filter #(= (second %) \S) (:data grid))))

(defn get-pipe-from-neighbors [coord grid]
  (reduce (fn [pipe [dir neighbor-coord]]
            (if-let [neighbor-tile (get-in grid [:data neighbor-coord])]
              (if (contains? (tile-to-pipe neighbor-tile) (opposite dir))
                (conj pipe dir)
                pipe)
              pipe))
          #{}
          (for [[dir delta] directions]
            [dir (add-coord coord delta)])))

(defn traverse-loop [start grid]
  (let [start-pipe (get-pipe-from-neighbors start grid)
        initial-dir (first start-pipe)]
    (loop [path [start]
           current (add-coord start (directions initial-dir))
           prev-dir initial-dir]
      (if (= current start)
        path
        (let [current-pipe (tile-to-pipe (get-in grid [:data current]))
              next-dir (first (disj current-pipe (opposite prev-dir)))]
          (recur (conj path current)
                 (add-coord current (directions next-dir))
                 next-dir))))))

(defn build-path-grid [grid path]
  (let [path-set (set path)
        path-data (select-keys (:data grid) path)
        start-coord (first path)
        start-tile (ffirst (filter #(= (second %) (get-pipe-from-neighbors start-coord grid))
                                   tile-to-pipe))]
    (assoc grid :data (assoc path-data start-coord start-tile))))

(defn inside? [[x y] grid]
  (if (contains? (:data grid) [x y])
    false
    (loop [x-pos 0
           crossings 0
           last-corner nil]
      (if (>= x-pos x)
        (odd? crossings)
        (case (get-in grid [:data [x-pos y]])
          \| (recur (inc x-pos) (inc crossings) last-corner)
          \L (recur (inc x-pos) crossings \L)
          \F (recur (inc x-pos) crossings \F)
          \J (recur (inc x-pos) 
                    (if (= last-corner \F) (inc crossings) crossings)
                    nil)
          \7 (recur (inc x-pos)
                    (if (= last-corner \L) (inc crossings) crossings)
                    nil)
          (recur (inc x-pos) crossings last-corner))))))

(defn solve [lines]
  (let [grid (parse-input lines)
        start (find-start grid)
        path (traverse-loop start grid)
        path-grid (build-path-grid grid path)]
    (count (for [y (range (:height grid))
                 x (range (:width grid))
                 :when (inside? [x y] path-grid)]
             1))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (println (solve (line-seq rdr)))))

(-main)
