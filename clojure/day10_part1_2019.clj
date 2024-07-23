
(ns asteroid-monitoring
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))

(defn parse-map [lines]
  (let [asteroids (for [y (range (count lines))
                        x (range (count (first lines)))
                        :let [char (nth (nth lines y) x)]
                        :when (= char \#)]
                    [x y])]
    asteroids))

(defn gcd [a b]
  (if (zero? b)
    (Math/abs a)
    (gcd b (mod a b))))

(defn slope [a b]
  (let [[x1 y1] a
        [x2 y2] b
        dx (- x2 x1)
        dy (- y2 y1)]
    (let [g (gcd dx dy)]
      [(quot dx g) (quot dy g)])))

(defn count-visible [asteroid asteroids]
  (let [slopes (atom #{})]
    (doseq [other asteroids :when (not= asteroid other)]
      (let [s (slope asteroid other)]
        (swap! slopes conj s)))
    (count @slopes)))

(defn best-location [asteroids]
  (apply max-key (fn [[_ count]] count)
         (for [asteroid asteroids]
           [asteroid (count-visible asteroid asteroids)])))

(defn -main []
  (let [lines (read-input "input.txt")
        asteroids (parse-map lines)
        [[best-x best-y] max-count] (best-location asteroids)]
    (println "Best location:" best-x "," best-y "with" max-count "other asteroids detected.")))

(-main)
