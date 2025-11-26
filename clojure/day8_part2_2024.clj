
(ns antenna
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn gcd [a b]
  (if (zero? b) (Math/abs a) (recur b (mod a b))))

(defn parse-grid [lines]
  (let [grid (vec lines) h (count grid) w (count (first grid))]
    [h w (reduce-kv
          (fn [acc y line]
            (reduce-kv
             (fn [acc x c]
               (if (= c \.) acc (update acc c (fnil conj []) [y x])))
             acc (vec line)))
          {} (vec (map vec grid)))]))

(defn normalize-dir [[dy dx]]
  (let [g (gcd dy dx) sy (/ dy g) sx (/ dx g)]
    (cond
      (neg? sx) [(- sy) (- sx)]
      (and (zero? sx) (neg? sy)) [(- sy) sx]
      :else [sy sx])))

(defn line-key [[ay ax] [by bx]]
  (let [[sy sx] (normalize-dir [(- by ay) (- bx ax)])]
    (str sx "," sy "," (- (* sy ax) (* sx ay)))))

(defn collect-lines [antennas]
  (reduce-kv
   (fn [acc freq coords]
     (assoc acc freq
            (reduce
             (fn [lines i]
               (reduce
                (fn [lines j]
                  (conj lines (line-key (nth coords i) (nth coords j))))
                lines (range (inc i) (count coords))))
             #{} (range (count coords)))))
   {} antennas))

(defn in-range? [[h w] [y x]] (and (< -1 y h) (< -1 x w)))

(defn collect-antinodes [h w lines]
  (reduce
   (fn [antinodes key]
     (let [[sx sy c] (map #(Long/parseLong %) (str/split key #","))]
       (cond
         (and (zero? sx) (zero? sy)) antinodes
         (zero? sy) (if (zero? (mod c sx))
                      (let [y (/ (- c) sx)]
                        (if (<= 0 y (dec h))
                          (reduce #(conj %1 (str y "," %2)) antinodes (range w))
                          antinodes))
                      antinodes)
         (zero? sx) (if (zero? (mod c sy))
                      (let [x (/ c sy)]
                        (if (<= 0 x (dec w))
                          (reduce #(conj %1 (str %2 "," x)) antinodes (range h))
                          antinodes))
                      antinodes)
         :else (reduce
                (fn [acc y]
                  (let [val (+ c (* sx y))]
                    (if (and (zero? (mod val sy))
                             (in-range? [h w] [y (/ val sy)]))
                      (conj acc (str y "," (/ val sy)))
                      acc)))
                antinodes (range h)))))
   #{} lines))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        [h w antennas] (parse-grid lines)
        lines (collect-lines antennas)
        all-lines (into #{} (mapcat val lines))
        antinodes (collect-antinodes h w all-lines)]
    (println (count antinodes))))

(-main)
