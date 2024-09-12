(ns santa-delivery
  (:require [clojure.string :as str]))

(defn move [[x y] direction]
  (case direction
    \^ [x (inc y)]
    \v [x (dec y)]
    \> [(inc x) y]
    \< [(dec x) y]
    [x y]))  ; Default case, shouldn't occur with valid input

(defn count-houses [directions]
  (let [initial-state {:position [0 0] :visited #{[0 0]}}]
    (count (:visited (reduce (fn [state dir]
                               (let [new-pos (move (:position state) dir)]
                                 {:position new-pos
                                  :visited (conj (:visited state) new-pos)}))
                             initial-state
                             directions)))))

(defn -main []
  (let [input (str/trim (slurp "input.txt"))
        result (count-houses input)]
    (println result)))

(-main)
