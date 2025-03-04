
(ns asteroid-monitoring
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (for [y (range (count lines))
          x (range (count (first lines)))
          :when (= \# (get (nth lines y) x))]
      [x y])))

(defn gcd [a b]
  (if (zero? b)
    (abs a)
    (recur b (rem a b))))

(defn visible-asteroids [asteroids [x0 y0]]
  (count
   (distinct
    (for [[x y] asteroids
          :when (not= [x y] [x0 y0])]
      (let [dx (- x x0)
            dy (- y y0)
            g (gcd dx dy)]
        [(/ dx g) (/ dy g)])))))

(defn best-location [asteroids]
  (apply max-key (partial visible-asteroids asteroids) asteroids))

(defn solve-part1 [input]
  (let [asteroids (parse-input input)
        best (best-location asteroids)]
    (visible-asteroids asteroids best)))

(defn angle [[x0 y0] [x y]]
  (let [dx (- x x0)
        dy (- y y0)]
    (mod (+ 90 (* (/ (Math/atan2 dy dx) Math/PI) 180)) 360)))

(defn vaporize-order [asteroids [x0 y0]]
  (let [others (remove #(= % [x0 y0]) asteroids)
        angled-asteroids (map (fn [a] [(angle [x0 y0] a) a]) others)
        grouped-asteroids (group-by first angled-asteroids)
        sorted-angles (sort (keys grouped-asteroids))]
    (->> sorted-angles
         (map (fn [angle]
                (->> (get grouped-asteroids angle)
                     (map second)
                     (sort-by (fn [[x y]] (+ (Math/abs (- x x0)) (Math/abs (- y y0))))))))
         (apply interleave)
         (remove nil?))))

(defn solve-part2 [input n]
  (let [asteroids (parse-input input)
        best (best-location asteroids)
        vaporization-order (vaporize-order asteroids best)
        nth-vaporized (nth vaporization-order (dec n))]
    (+ (* (first nth-vaporized) 100) (second nth-vaporized))))

(defn main []
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input 200))))

(main)
