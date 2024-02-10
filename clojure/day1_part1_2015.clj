
(def input (slurp "input.txt"))

(def floor (reduce + (map #(if (= % \() 1 -1) input)))

(println floor)
