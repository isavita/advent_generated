
(ns g
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-grid [lines]
  (let [grid (mapv vec lines)
        h    (count grid)
        w    (count (first grid))]
    (loop [y 0 x 0]
      (when (< y h)
        (case (get-in grid [y x])
          \^ [x y 0]
          \> [x y 1]
          \v [x y 2]
          \< [x y 3]
          (recur (if (= x (dec w)) (inc y) y)
                 (if (= x (dec w)) 0 (inc x))))))))

(defn loops? [grid sx sy sdir h w]
  (let [dirs  [[0 -1] [1 0] [0 1] [-1 0]]
        seen  (java.util.HashSet.)]
    (loop [x sx y sy dir sdir steps 20000]
      (when (pos? steps)
        (let [state (bit-or (bit-shift-left (bit-or (bit-shift-left y 16) x) 2) dir)]
          (if (.contains seen state)
            true
              (do (.add seen state)
                  (let [[dx dy] (nth dirs dir)
                        nx      (+ x dx)
                        ny      (+ y dy)]
                    (cond
                      (or (neg? nx) (>= nx w) (neg? ny) (>= ny h)) nil
                      (= \# (get-in grid [ny nx]))
                      (recur x y (mod (inc dir) 4) (dec steps))
                      :else
                      (recur nx ny dir (dec steps)))))))))))

(defn -main [& _]
  (let [lines   (str/split-lines (slurp "input.txt"))
        [sx sy start-dir] (parse-grid lines)
        grid    (mapv vec lines)
        h       (count grid)
        w       (count (first grid))
        grid    (assoc-in grid [sy sx] \.)
        cnt     (atom 0)]
    (doseq [y (range h) x (range w)
            :when (and (not= [x y] [sx sy])
                       (= \. (get-in grid [y x])))]
      (when (loops? (assoc-in grid [y x] \#) sx sy start-dir h w)
        (swap! cnt inc)))
    (println @cnt)))

(-main)
