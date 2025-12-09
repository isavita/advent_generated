
(ns tda
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ITERATIONS 50)
(def EXPAND-BY 1)

(defn read-input [filename]
  (let [lines (str/split-lines (slurp filename))
        algo (first lines)
        img (mapv #(mapv (fn [c] (= c \#)) %) (filter seq (rest lines)))]
    [algo img]))

(defn enhance-image [algo img use-infinite-lit]
  (let [h (count img)
        w (count (first img))
        new-h (+ h (* EXPAND-BY 2))
        new-w (+ w (* EXPAND-BY 2))
        new-img (vec (repeat new-h (vec (repeat new-w false))))]
    (reduce
      (fn [g [y x]]
        (let [idx (reduce
                    (fn [acc [dy dx]]
                      (let [ny (+ y dy)
                            nx (+ x dx)
                            bit (if (and (>= ny 0) (< ny h)
                                         (>= nx 0) (< nx w))
                                  (get-in img [ny nx])
                                  use-infinite-lit)]
                        (+ (bit-shift-left acc 1) (if bit 1 0))))
                    0
                    (for [dy (range -1 2) dx (range -1 2)] [dy dx]))]
          (assoc-in g [(+ y EXPAND-BY) (+ x EXPAND-BY)] (= (nth algo idx) \#))))
      new-img
      (for [y (range (- EXPAND-BY) (+ h EXPAND-BY))
            x (range (- EXPAND-BY) (+ w EXPAND-BY))]
        [y x]))))

(defn count-lit-pixels [img]
  (transduce (map #(count (filter true? %))) + img))

(defn -main [& _]
  (let [[algo img] (read-input "input.txt")
        final-img (reduce
                    (fn [img i]
                      (enhance-image algo img (and (odd? i) (= (first algo) \#))))
                    img
                    (range ITERATIONS))]
    (println (count-lit-pixels final-img))))

(-main)
