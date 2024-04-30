(ns nanobots
  (:require [clojure.string :as str]
             [clojure.java.io :as io]
             [clojure.core :as core]))

(defn parse-nanobots [file]
  (with-open [rdr (io/reader file)]
    (doall (map (fn [line]
                   (let [[_ x y z r] (re-matches #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)]
                     {:x (Integer/parseInt x) :y (Integer/parseInt y) :z (Integer/parseInt z) :radius (Integer/parseInt r)}))
                 (line-seq rdr)))))

(defn find-strongest-nanobot [nanobots]
  (apply max-key :radius nanobots))

(defn manhattan-distance [a b]
  (+ (Math/abs (- (:x a) (:x b))) (Math/abs (- (:y a) (:y b))) (Math/abs (- (:z a) (:z b)))))

(defn count-nanobots-in-range [nanobots strongest]
  (count (filter #(<= (manhattan-distance % strongest) (:radius strongest)) nanobots)))

(defn -main []
  (let [nanobots (parse-nanobots "input.txt")
        strongest (find-strongest-nanobot nanobots)
        in-range-count (count-nanobots-in-range nanobots strongest)]
    (println in-range-count)))

(-main)