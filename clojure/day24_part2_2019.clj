
(ns main
  (:require [clojure.string :as str]))

(def side 5)
(def square (* side side))

(defn parse []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [lines (line-seq rdr)]
      (vec (for [row (range side)
                 col (range side)]
             (= \# (nth (nth lines row) col)))))))

(defn neighbours [space level cell]
  (let [row (quot cell side)
        col (mod cell side)
        lvl #(get-in space [%1 %2] false)]
    (+ (if (and (= row 0) (lvl (dec level) 7)) 1 0)
       (if (and (= col 0) (lvl (dec level) 11)) 1 0)
       (if (and (= col (dec side)) (lvl (dec level) 13)) 1 0)
       (if (and (= row (dec side)) (lvl (dec level) 17)) 1 0)
       (if (= cell 7) (count (filter identity (for [i (range side)] (lvl (inc level) i)))) 0)
       (if (= cell 11) (count (filter identity (for [i (range side)] (lvl (inc level) (* i side))))) 0)
       (if (= cell 13) (count (filter identity (for [i (range side)] (lvl (inc level) (+ (* i side) (dec side)))))) 0)
       (if (= cell 17) (count (filter identity (for [i (range side)] (lvl (inc level) (+ (* (dec side) side) i))))) 0)
       (if (and (> row 0) (not= cell 17) (lvl level (- cell side))) 1 0)
       (if (and (> col 0) (not= cell 13) (lvl level (dec cell))) 1 0)
       (if (and (< col (dec side)) (not= cell 11) (lvl level (inc cell))) 1 0)
       (if (and (< row (dec side)) (not= cell 7) (lvl level (+ cell side))) 1 0))))

(defn next2 [space]
  (let [min-level (apply min (keys space))
        max-level (apply max (keys space))
        new-space (atom {})]
    (doseq [level (range (dec min-level) (+ 2 max-level))]
      (swap! new-space assoc level (vec (repeat square false)))
      (doseq [cell (range square)]
        (when (not= cell 12)
          (let [n (neighbours space level cell)
                cur (get-in space [level cell] false)]
            (cond
              (and cur (not= n 1)) (swap! new-space assoc-in [level cell] false)
              (and (not cur) (or (= n 1) (= n 2))) (swap! new-space assoc-in [level cell] true)
              :else (swap! new-space assoc-in [level cell] cur))))))
    (let [cleaned (into {} (filter (fn [[k v]] (some true? v)) @new-space))]
      cleaned)))

(defn -main []
  (let [initial (parse)
        space (loop [s {0 initial} i 0]
                (if (< i 200)
                  (recur (next2 s) (inc i))
                  s))
        total (transduce (comp (map val) (map #(count (filter identity %)))) + space)]
    (println total)))

(-main)
