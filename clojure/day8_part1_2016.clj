(ns day8
  (:require [clojure.java.io :as io]
             [clojure.string :as str]))

(def screen-width 50)
(def screen-height 6)

(defn rect [screen a b]
  (doseq [y (range b) x (range a)]
    (aset screen y x true)))

(defn rotate-row [screen row shift]
  (let [temp (make-array Boolean/TYPE screen-width)]
    (dotimes [i screen-width]
      (aset temp (mod (+ i shift) screen-width) (aget screen row i)))
    (aset screen row temp)))

(defn rotate-column [screen col shift]
  (let [temp (make-array Boolean/TYPE screen-height)]
    (dotimes [i screen-height]
      (aset temp (mod (+ i shift) screen-height) (aget screen i col)))
    (dotimes [i screen-height]
      (aset screen i col (aget temp i)))))

(defn count-lit-pixels [screen]
  (reduce + (map (fn [row] (count (filter true? row))) screen)))

(defn process-instruction [instruction screen]
  (let [[_ a b] (re-matches #"rect (\d+)x(\d+)" instruction)]
    (if a
      (rect screen (Integer/parseInt a) (Integer/parseInt b))
      (let [[_ a b] (re-matches #"rotate row y=(\d+) by (\d+)" instruction)]
        (if a
          (rotate-row screen (Integer/parseInt a) (Integer/parseInt b))
          (let [[_ a b] (re-matches #"rotate column x=(\d+) by (\d+)" instruction)]
            (rotate-column screen (Integer/parseInt a) (Integer/parseInt b))))))))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [screen (make-array Boolean/TYPE screen-height screen-width)]
      (doseq [line (line-seq r)]
        (process-instruction line screen))
      (println (count-lit-pixels screen)))))

(-main)