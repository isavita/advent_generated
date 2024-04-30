(ns knot-tying
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn knot-tying [lengths]
  (let [list (vec (range 256))
        lengths (map #(Integer/parseInt %) lengths)]
    (loop [list list
           current-position 0
           skip-size 0
           lengths lengths]
      (if (empty? lengths)
        (* (nth list 0) (nth list 1))
        (let [length (first lengths)
              list (loop [i 0
                          list list]
                     (if (>= i (quot length 2))
                       list
                       (recur (inc i)
                              (let [start (mod (+ current-position i) 256)
                                    end (mod (+ current-position (- length 1 i)) 256)]
                                (assoc list start (nth list end) end (nth list start)))))
                     )
              current-position (mod (+ current-position length skip-size) 256)
              skip-size (inc skip-size)
              lengths (rest lengths)]
          (recur list current-position skip-size lengths))))))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [line (.readLine r) ; Java interop to read a line
          lengths (str/split line #",")]
      (println (knot-tying lengths)))))

(-main)