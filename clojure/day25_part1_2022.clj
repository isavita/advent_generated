
(defn snafu->decimal [snafu]
  (reduce (fn [acc digit]
            (let [value (case digit
                          \= -2
                          \- -1
                          \0 0
                          \1 1
                          \2 2)]
              (+ (* acc 5) value)))
          0
          snafu))

(defn decimal->snafu [decimal]
  (if (zero? decimal)
    ""
    (let [remainder (rem decimal 5)
          next-decimal (quot decimal 5)]
      (case remainder
        0 (str (decimal->snafu next-decimal) "0")
        1 (str (decimal->snafu next-decimal) "1")
        2 (str (decimal->snafu next-decimal) "2")
        3 (str (decimal->snafu (inc next-decimal)) "=")
        4 (str (decimal->snafu (inc next-decimal)) "-")))))

(defn solve []
  (let [snafu-numbers (-> "input.txt"
                          slurp
                          (clojure.string/split-lines))
        decimal-sum (reduce + (map snafu->decimal snafu-numbers))]
    (println (decimal->snafu decimal-sum))))

(solve)
