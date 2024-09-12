(defn solve-maze [instructions]
  (loop [pos 0
         steps 0
         maze instructions]
    (if (or (< pos 0) (>= pos (count maze)))
      steps
      (let [jump (get maze pos)]
        (recur (+ pos jump)
               (inc steps)
               (assoc maze pos (inc jump)))))))

(defn read-input [file]
  (vec (map #(Integer/parseInt %) (clojure.string/split-lines (slurp file)))))

(defn -main []
  (let [input (read-input "input.txt")
        result (solve-maze input)]
    (println result)))

(-main)
