
(defn parse-instruction [line]
  (let [[op a b] (clojure.string/split line #" ")]
    (cond
      (or (= op "inc") (= op "dec")) [op a]
      (= op "jnz") [op a (Integer/parseInt b)]
      :else [op a b])))

(defn get-value [x registers]
  (if (re-matches #"-?\d+" x)
    (Integer/parseInt x)
    (get registers x 0)))

(defn execute [instructions registers]
  (loop [i 0
         registers registers]
    (if (>= i (count instructions))
      registers
      (let [instruction (nth instructions i)
            op (first instruction)]
        (case op
          "cpy" (recur (inc i) (assoc registers (nth instruction 2) (get-value (nth instruction 1) registers)))
          "inc" (recur (inc i) (update registers (nth instruction 1) inc))
          "dec" (recur (inc i) (update registers (nth instruction 1) dec))
          "jnz" (if (not= 0 (get-value (nth instruction 1) registers))
                  (recur (+ i (nth instruction 2)) registers)
                  (recur (inc i) registers)))))))

(defn solve []
  (let [instructions (->> "input.txt"
                          slurp
                          clojure.string/split-lines
                          (map parse-instruction))
        registers (execute instructions {"a" 0 "b" 0 "c" 1 "d" 0})]
    (println (get registers "a"))))

(solve)
