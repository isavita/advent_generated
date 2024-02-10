
(defn move [direction [x y]]
  (cond
    (= direction \^) [x (inc y)]
    (= direction \v) [x (dec y)]
    (= direction \>) [(inc x) y]
    (= direction \<) [(dec x) y]))

(defn houses-visited [input]
  (let [directions (seq input)
        visited (reduce (fn [acc direction]
                          (let [new-pos (move direction (last acc))]
                            (conj acc new-pos)))
                        [[0 0]]
                        directions)]
    (count (distinct visited))))

(-> (slurp "input.txt")
    (houses-visited)
    (println))
