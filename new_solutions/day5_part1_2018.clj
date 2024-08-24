(defn react [polymer]
  (loop [units (seq polymer)
         stack []]
    (if (empty? units)
      (apply str stack)
      (let [current (first units)
            remaining (rest units)]
        (if (and (not (empty? stack))
                 (= (clojure.string/lower-case current) (clojure.string/lower-case (peek stack)))
                 (not= current (peek stack)))
          (recur remaining (pop stack))
          (recur remaining (conj stack current)))))))

(defn -main []
  (let [input (slurp "input.txt")
        reacted (react input)]
    (println (count reacted))))

(-main)