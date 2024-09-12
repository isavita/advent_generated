(ns maze-solver
  (:require [clojure.string :as str]))

(defn solve [jumps part]
  (loop [pos 0
         steps 0
         jumps (transient jumps)]
    (if (or (< pos 0) (>= pos (count jumps)))
      steps
      (let [jump (jumps pos)
            new-jump (if (= part 2)
                       (if (>= jump 3) (dec jump) (inc jump))
                       (inc jump))]
        (recur (+ pos jump)
               (inc steps)
               (assoc! jumps pos new-jump))))))

(defn -main []
  (let [jumps (mapv parse-long (str/split-lines (slurp "input.txt")))]
    (println "Part 1:" (solve jumps 1))
    (println "Part 2:" (solve jumps 2))))

(-main)
