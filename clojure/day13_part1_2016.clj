
(ns day13
  (:require [clojure.java.io :as io]))

(defn- is-open-space? [x y favorite-number]
  (let [value (+ (* x x) (* 3 x) (* 2 x y) y (* y y) favorite-number)
        binary-string (Integer/toBinaryString value)
        ones-count (count (filter #(= \1 %) binary-string))]
    (even? ones-count)))

(defn solve [favorite-number target-x target-y]
  (let [start-x 1
        start-y 1]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start-x start-y 0])
           visited #{[start-x start-y]}]
      (if (empty? q)
        -1 ; Target not reachable
        (let [[x y steps] (peek q)
              q (pop q)]
          (if (and (= x target-x) (= y target-y))
            steps
            (let [neighbors [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
                  valid-neighbors (filter (fn [[nx ny]]
                                            (and (>= nx 0)
                                                 (>= ny 0)
                                                 (is-open-space? nx ny favorite-number)
                                                 (not (contains? visited [nx ny]))))
                                          neighbors)
                  new-visited (reduce conj visited (map #(vec (take 2 %)) valid-neighbors))
                  new-q (reduce conj q (map #(conj % (inc steps)) valid-neighbors))]
              (recur new-q new-visited))))))))


(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [favorite-number (Integer/parseInt (first (line-seq rdr)))]
      (println (solve favorite-number 31 39)))))

(-main)
