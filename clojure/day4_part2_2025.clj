
(require '[clojure.string :as str])

(defn -main [& _]
  (let [lines   (str/split-lines (slurp "input.txt"))
        R       (count lines)
        C       (if (zero? R) 0 (apply max (map count lines)))
        grid    (vec (map vec lines))
        dr      [-1 -1 -1  0 0 1 1 1]
        dc      [-1  0  1 -1 1 -1 0 1]]
    (if (or (zero? R) (zero? C))
      (println "Total rolls removed: 0")
      (loop [grid grid removed 0]
        (let [to-remove
              (for [r (range R) c (range C)
                    :when (= \@ (get-in grid [r c]))
                    :when (< (count (filter #(= \@ (get-in grid %))
                                            (map #(vector (+ r %1) (+ c %2)) dr dc)))
                             4)]
                [r c])
              new-grid (reduce #(assoc-in %1 %2 \.) grid to-remove)]
          (if (empty? to-remove)
            (println "Total rolls removed:" removed)
            (recur new-grid (+ removed (count to-remove)))))))))

(-main)
