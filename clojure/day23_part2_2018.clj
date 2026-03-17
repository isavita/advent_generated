
(ns nanobots.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util PriorityQueue]))

(defn parse-input [filename]
  (with-open [rdr (io/reader filename)]
    (doall
      (for [line (line-seq rdr)
            :let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)]]
        {:x (Integer/parseInt x)
         :y (Integer/parseInt y)
         :z (Integer/parseInt z)
         :r (Integer/parseInt r)}))))

(defn manhattan-distance [a b]
  (+ (Math/abs (- (:x a) (:x b)))
     (Math/abs (- (:y a) (:y b)))
     (Math/abs (- (:z a) (:z b)))))

(defn part-one [bots]
  (let [strongest (apply max-key :r bots)]
    (count (filter #(<= (manhattan-distance strongest %) (:r strongest)) bots))))

(defn min-distance-to-origin [x y z size]
  (let [dx (cond (> x 0) x
                 (< (+ x size -1) 0) (- (+ x size -1))
                 :else 0)
        dy (cond (> y 0) y
                 (< (+ y size -1) 0) (- (+ y size -1))
                 :else 0)
        dz (cond (> z 0) z
                 (< (+ z size -1) 0) (- (+ z size -1))
                 :else 0)]
    (+ dx dy dz)))

(defn count-bots-in-range [bots x y z size]
  (count
    (for [bot bots
          :let [dx (cond (< (:x bot) x) (- x (:x bot))
                         (> (:x bot) (+ x size -1)) (- (:x bot) (+ x size -1))
                         :else 0)
                dy (cond (< (:y bot) y) (- y (:y bot))
                         (> (:y bot) (+ y size -1)) (- (:y bot) (+ y size -1))
                         :else 0)
                dz (cond (< (:z bot) z) (- z (:z bot))
                         (> (:z bot) (+ z size -1)) (- (:z bot) (+ z size -1))
                         :else 0)
                d (+ dx dy dz)]
          :when (<= d (:r bot))]
      bot)))

(defn part-two [bots]
  (let [min-x (apply min (map :x bots))
        max-x (apply max (map :x bots))
        min-y (apply min (map :y bots))
        max-y (apply max (map :y bots))
        min-z (apply min (map :z bots))
        max-z (apply max (map :z bots))
        size (loop [s 1]
               (if (< s (max (- max-x min-x) (- max-y min-y) (- max-z min-z)))
                 (recur (* 2 s))
                 s))
        pq (PriorityQueue.
              (fn [[c1 d1 s1 _ _ _] [c2 d2 s2 _ _ _]]
                (cond
                  (not= c1 c2) (compare c1 c2)
                  (not= d1 d2) (compare d1 d2)
                  :else (compare s2 s1))))]
    
    (.add pq [0 (min-distance-to-origin min-x min-y min-z size) size min-x min-y min-z])
    
    (loop []
      (when-let [[neg-count distance sz x y z] (.poll pq)]
        (let [count (- neg-count)]
          (if (= sz 1)
            distance
            (let [half (quot sz 2)]
              (doseq [dx [0 half] dy [0 half] dz [0 half]]
                (let [nx (+ x dx) ny (+ y dy) nz (+ z dz)
                      new-size (max 1 half)
                      cnt (count-bots-in-range bots nx ny nz new-size)
                      new-dist (min-distance-to-origin nx ny nz new-size)]
                  (.add pq [(- cnt) new-dist new-size nx ny nz])))
              (recur))))))))

(defn -main []
  (let [bots (parse-input "input.txt")]
    (println "Part One:" (part-one bots))
    (println "Part Two:" (part-two bots))))

(-main)
