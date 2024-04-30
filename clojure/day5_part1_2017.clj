(defn maze-escape
  [instructions]
  (loop [instructions (vec instructions)
         index 0
         steps 0]
    (if (or (< index 0) (>= index (count instructions)))
      steps
      (recur (assoc instructions index (inc (nth instructions index)))
             (+ index (nth instructions index))
             (inc steps)))))

(defn -main
  []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [instructions (map read-string (line-seq rdr))]
      (println (maze-escape instructions)))))

(-main)