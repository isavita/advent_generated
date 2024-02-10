
(def input (slurp "input.txt"))

(defn floor [input]
  (reduce + (map #(if (= % \() 1 -1) input)))

(defn basement-position [input]
  (loop [pos 0 floor 0]
    (if (neg? floor)
      pos
      (recur (inc pos) (+ floor (if (= (nth input pos) \() 1 -1))))))

(println (floor input))
(println (basement-position input))
