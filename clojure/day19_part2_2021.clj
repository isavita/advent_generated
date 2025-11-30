
(ns day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-int [s] (Long/parseLong s))

(def rotations
  [[identity
    (fn [[x y z]] [x (- z) y])
    (fn [[x y z]] [x (- y) (- z)])
    (fn [[x y z]] [x z (- y)])
    (fn [[x y z]] [(- x) (- y) z])
    (fn [[x y z]] [(- x) (- z) (- y)])
    (fn [[x y z]] [(- x) y (- z)])
    (fn [[x y z]] [(- x) z y])
    (fn [[x y z]] [y (- x) z])
    (fn [[x y z]] [y (- z) (- x)])
    (fn [[x y z]] [y x (- z)])
    (fn [[x y z]] [y z x])
    (fn [[x y z]] [(- y) x z])
    (fn [[x y z]] [(- y) (- z) x])
    (fn [[x y z]] [(- y) (- x) (- z)])
    (fn [[x y z]] [(- y) z (- x)])
    (fn [[x y z]] [z y (- x)])
    (fn [[x y z]] [z x y])
    (fn [[x y z]] [z (- y) x])
    (fn [[x y z]] [z (- x) (- y)])
    (fn [[x y z]] [(- z) y x])
    (fn [[x y z]] [(- z) (- x) y])
    (fn [[x y z]] [(- z) (- y) (- x)])
    (fn [[x y z]] [(- z) x (- y)])]])

(defn read-input [file]
  (->> (slurp file)
       str/split-lines
       (reduce (fn [[scanners curr] line]
                 (cond
                   (str/starts-with? line "---")
                   (if (empty? curr)
                     [scanners curr]
                     [(conj scanners curr) []])
                   (empty? line)
                   [scanners curr]
                   :else
                   [scanners (conj curr (mapv parse-int (str/split line #",")))]))
               [[] []])
       ((fn [[scanners curr]] (if (empty? curr) scanners (conj scanners curr))))))

(defn sub-point [[x1 y1 z1] [x2 y2 z2]]
  [(- x1 x2) (- y1 y2) (- z1 z2)])

(defn add-point [[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])

(defn manhattan [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))
     (Math/abs (- z1 z2))))

(defn find-overlap [base scanner]
  (let [base-set (set base)]
    (loop [r 0]
      (when (< r 24)
        (let [rot (nth (first rotations) r)
              rotated (mapv rot scanner)
              freqs (frequencies
                     (for [p1 base
                           p2 rotated]
                       (sub-point p1 p2)))]
          (if-let [[delta cnt] (first (sort-by (comp - val) freqs))]
            (if (>= cnt 12)
              (let [translated (mapv #(add-point % delta) rotated)]
                [translated delta])
              (recur (inc r)))
            (recur (inc r))))))))

(defn solve [scanners]
  (loop [beacons (set (first scanners))
         positions #{[0 0 0]}
         remaining (vec (rest scanners))]
    (if (empty? remaining)
      [beacons positions]
      (let [scanner (first remaining)]
        (if-let [[translated pos] (find-overlap (vec beacons) scanner)]
          (recur (into beacons translated) (conj positions pos) (vec (rest remaining)))
          (recur beacons positions (conj (vec (rest remaining)) scanner)))))))

(defn -main []
  (let [scanners (read-input "input.txt")
        [beacons positions] (solve scanners)]
    (println (count beacons))
    (println (apply max (for [a positions b positions] (manhattan a b))))))

(-main)
