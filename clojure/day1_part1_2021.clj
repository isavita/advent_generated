
(def input (slurp "input.txt"))

(def depths (map read-string (clojure.string/split-lines input)))

(def larger-count (count (filter #(> % 0) (map - (rest depths) depths))))

(println larger-count)
