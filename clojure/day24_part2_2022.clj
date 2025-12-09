
(ns blizzard
  (:require [clojure.string :as str])
  (:gen-class))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b)) 1 (/ (* a b) (gcd a b))))

(defn pos-mod [i n]
  (mod (mod i n) n))

(defn read-input [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (let [lines (line-seq rdr)
          height (count lines)
          width (count (first lines))
          walls (into [] (for [y (range height)]
                           (into [] (for [x (range width)]
                                      (= \# (get-in lines [y x]))))))
          blizzards (for [y (range height) x (range width)
                          :let [c (get-in lines [y x])]
                          :when (contains? #{\> \< \v \^} c)]
                      {:x x :y y :dir (case c \> 0 \< 1 \v 2 \^ 3)})]
      {:walls walls :blizzards blizzards :height height :width width})))

(defn find-start-end [{:keys [walls height width]}]
  (let [start-x (first (keep-indexed (fn [x w] (when-not w x)) (first walls)))
        end-x (first (keep-indexed (fn [x w] (when-not w x)) (last walls)))]
    {:start {:x start-x :y 0} :end {:x end-x :y (dec height)}}))

(defn precompute-blizzards [{:keys [blizzards height width]}]
  (let [inner-width (- width 2)
        inner-height (- height 2)
        period (lcm inner-width inner-height)
        blizzard-pos (into [] (for [t (range period)]
                                (into [] (for [y (range height)]
                                           (into [] (for [x (range width)] false))))))]
    (doseq [t (range period)
            {:keys [x y dir]} blizzards
            :let [nx (case dir
                       0 (inc (pos-mod (dec x) inner-width))
                       1 (inc (pos-mod (- (dec x) t) inner-width))
                       2 x
                       3 x)
                  ny (case dir
                       0 y
                       1 y
                       2 (inc (pos-mod (dec y) inner-height))
                       3 (inc (pos-mod (- (dec y) t) inner-height))))]
      (when (and (< -1 nx width) (< -1 ny height))
        (aset-boolean (blizzard-pos t) ny nx true)))
    {:period period :blizzard-pos blizzard-pos}))

(defn bfs [{:keys [walls height width]} blizzard-data start end start-time]
  (let [{:keys [period blizzard-pos]} blizzard-data
        visited (into [] (for [y (range height)]
                           (into [] (for [x (range width)]
                                      (into [] (for [t (range period)] false))))))
        queue (java.util.ArrayDeque.)
        dx [0 1 -1 0 0]
        dy [0 0 0 1 -1]]
    (aset-boolean visited (:y start) (:x start) (mod start-time period) true)
    (.add queue [(:x start) (:y start) start-time])
    (while (not (.isEmpty queue))
      (let [[x y t] (.poll queue)
            next-t (inc t)
            next-t-mod (mod next-t period)]
        (doseq [i (range 5)]
          (let [nx (+ x (dx i))
                ny (+ y (dy i))]
            (when (and (< -1 nx width) (< -1 ny height)
                       (not (walls ny nx))
                       (not (blizzard-pos next-t-mod ny nx))
                       (not (visited ny nx next-t-mod)))
              (if (and (= nx (:x end)) (= ny (:y end)))
                (do (.clear queue) (return next-t))
                (do (aset-boolean visited ny nx next-t-mod true)
                    (.add queue [nx ny next-t]))))))))))

(defn -main [& args]
  (let [input (read-input "input.txt")
        {:keys [start end]} (find-start-end input)
        blizzard-data (precompute-blizzards input)
        time1 (bfs input blizzard-data start end 0)
        time2 (bfs input blizzard-data end start time1)
        time3 (bfs input blizzard-data start end time2)]
    (println time3)))
