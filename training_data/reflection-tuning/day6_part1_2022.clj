(defn all-different? [chars]
  (= (count chars) (count (distinct chars))))

(defn find-marker [datastream]
  (let [windows (partition 4 1 datastream)]
    (+ 4 (count (take-while #(not (all-different? %)) windows)))))

(defn solve-puzzle []
  (let [input (slurp "input.txt")]
    (println (find-marker input))))

(solve-puzzle)
