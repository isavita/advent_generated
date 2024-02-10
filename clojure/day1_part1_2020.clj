
(defn find-multiply []
  (let [numbers (mapv #(Integer/parseInt %) (clojure.string/split-lines (slurp "input.txt")))]
    (let [result (first (for [x numbers y numbers :when (= (+ x y) 2020)] (* x y)))]
      (println result))))

(find-multiply)
