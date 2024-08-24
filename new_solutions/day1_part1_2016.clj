(ns solution
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (slurp filename))

(defn solve []
  (let [input (read-input "input.txt")
        directions (str/split input #", ")
        dir (atom 0)
        x (atom 0)
        y (atom 0)]
    (doseq [d directions]
      (let [turn (first d)
            distance (Integer/parseInt (subs d 1))]
        (if (= turn \R)
          (swap! dir #(mod (+ % 1) 4))
          (swap! dir #(mod (- % 1) 4)))
        (case @dir
          0 (swap! y #(+ % distance))
          1 (swap! x #(+ % distance))
          2 (swap! y #(- % distance))
          3 (swap! x #(- % distance)))))
    (println (+ (Math/abs @x) (Math/abs @y)))))

(solve)