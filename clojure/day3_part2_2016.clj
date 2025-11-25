
(ns triangle
  (:require [clojure.string :as str]))

(defn valid? [[a b c]]
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ a c) b)))

(defn -main []
  (let [lines     (str/split-lines (slurp "input.txt"))
        numbers   (mapv #(mapv read-string (re-seq #"\d+" %)) lines)
        columns   (for [i (range 0 (count numbers) 3)
                        j (range 3)]
                    [(get-in numbers [i j])
                     (get-in numbers [(inc i) j])
                     (get-in numbers [(+ i 2) j])])]
    (println (count (filter valid? columns)))))

(-main)
