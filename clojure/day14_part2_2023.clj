
(ns parabolic-reflector
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:const empty-cell \.)
(def ^:const cubic-rock \#)
(def ^:const round-rock \O)

(defn parse-grid [lines]
  (let [width (count (first lines))
        height (count lines)]
    {:width width
     :height height
     :data (into-array Character/TYPE
                      (for [y (range height)
                            x (range width)]
                        (let [c (nth (nth lines y) x)]
                          (if (= c empty-cell) \0 c))))}))

(defn idx [grid x y]
  (+ x (* y (:width grid))))

(defn get-cell [grid x y]
  (aget ^chars (:data grid) (idx grid x y)))

(defn set-cell! [grid x y v]
  (aset ^chars (:data grid) (idx grid x y) v))

(defn in-bounds? [grid x y]
  (and (>= x 0) (< x (:width grid))
       (>= y 0) (< y (:height grid))))

(defn shift-single-rock! [grid x y dx dy]
  (when (= (get-cell grid x y) round-rock)
    (loop [cx x cy y]
      (let [bx (+ cx dx)
            by (+ cy dy)]
        (if (and (in-bounds? grid bx by)
                 (= (get-cell grid bx by) \0))
          (do (set-cell! grid bx by round-rock)
              (set-cell! grid cx cy \0)
              (recur bx by))
          nil)))))

(defn shift-rocks! [grid dx dy]
  (if (or (< dy 0) (< dx 0))
    (doseq [x (range (:width grid))
            y (range (:height grid))]
      (shift-single-rock! grid x y dx dy))
    (doseq [x (range (dec (:width grid)) -1 -1)
            y (range (dec (:height grid)) -1 -1)]
      (shift-single-rock! grid x y dx dy))))

(defn cycle-rocks! [grid]
  (shift-rocks! grid 0 -1)
  (shift-rocks! grid -1 0)
  (shift-rocks! grid 0 1)
  (shift-rocks! grid 1 0))

(defn grid-key [grid]
  (reduce +
    (for [x (range (:width grid))
          y (range (:height grid))
          :when (= (get-cell grid x y) round-rock)]
      (+ x (* y (:width grid))))))

(defn calculate-load [grid]
  (reduce +
    (for [x (range (:width grid))
          y (range (:height grid))
          :when (= (get-cell grid x y) round-rock)]
      (- (:height grid) y))))

(defn solve [lines]
  (let [grid (parse-grid lines)
        cache (atom {})
        num-cycles 1000000000]
    (loop [i 0]
      (if (= i num-cycles)
        (calculate-load grid)
        (let [k (grid-key grid)]
          (if-let [prev (@cache k)]
            (let [cycle-len (- i prev)
                  rem (mod (- num-cycles i) cycle-len)]
              (dotimes [_ rem] (cycle-rocks! grid))
              (calculate-load grid))
            (do (swap! cache assoc k i)
                (cycle-rocks! grid)
                (recur (inc i)))))))))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))]
    (println (solve lines))))

(-main)
