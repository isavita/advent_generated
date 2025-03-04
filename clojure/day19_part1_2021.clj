
(ns beacon-scanner
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-vector [s]
  (mapv #(Integer/parseInt %) (str/split s #",")))

(defn parse-scanner [scanner-str]
  (let [lines (str/split-lines scanner-str)
        beacons (map parse-vector (rest lines))]
    beacons))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map parse-scanner)))

(defn rotations [v]
  (let [[x y z] v]
    [
     [x y z] [x (- z) y] [x (- y) (- z)] [x z (- y)]
     [(- x) y (- z)] [(- x) z y] [(- x) (- y) z] [(- x) (- z) (- y)]
     [y (- x) z] [y (- z) (- x)] [y x (- z)] [y z x]
     [(- y) x z] [(- y) (- z) x] [(- y) (- x) (- z)] [(- y) z (- x)]
     [z y (- x)] [z x y] [z (- y) x] [z (- x) (- y)]
     [(- z) y x] [(- z) (- x) y] [(- z) (- y) (- x)] [(- z) x (- y)]
     ]))


(defn all-rotations [beacons]
  (->> (for [beacon beacons]
         (rotations beacon))
       (apply map vector)  ; transpose
       (map set)
       ))

(defn diff [v1 v2]
  (mapv - v1 v2))

(defn add [v1 v2]
  (mapv + v1 v2))


(defn solve [scanners]
  (let [scanner-positions (atom [[0 0 0]])]
    (loop [remaining-scanners (vec (rest scanners))
           oriented-scanners [(first scanners)]]
        (if (empty? remaining-scanners)
          (do
            (->> (for [scanner oriented-scanners
                       beacon scanner]
                   beacon)
            (distinct)
            (count)
            )
            )
          (let [
                [aligned-scanner transformed-scanner scanner-pos]
                (first
                  (for [aligned-scanner oriented-scanners
                        transformed-beacons (all-rotations (first remaining-scanners))
                        aligned-beacon aligned-scanner
                        transformed-beacon transformed-beacons
                        :let [scanner-pos (diff aligned-beacon transformed-beacon)
                              translated-scanner (map #(add % scanner-pos) transformed-beacons)]
                        :when (>= (count (set/intersection (set aligned-scanner) (set translated-scanner))) 12)]
                    [aligned-scanner translated-scanner scanner-pos]))]

             (if aligned-scanner
                (do
                  (swap! scanner-positions conj scanner-pos)
                  (recur (vec (rest remaining-scanners)) (conj oriented-scanners transformed-scanner))
                  )
                (recur (conj (vec (rest remaining-scanners)) (first remaining-scanners)) oriented-scanners))
            )
        ))))

(defn -main []
  (let [input (slurp "input.txt")
        scanners (parse-input input)]
    (println (solve scanners))))

(-main)
