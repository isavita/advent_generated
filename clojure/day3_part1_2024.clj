
(defn solve []
  (->> (slurp "input.txt")
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (fn [[_ x y]] (* (Integer/parseInt x) (Integer/parseInt y))))
       (reduce + 0)
       (println)))

(solve)
