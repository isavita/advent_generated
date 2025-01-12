
(defn next-secret [s]
  (let [x (* s 64)
        s (bit-xor s x)
        s (bit-and s 0xFFFFFF)
        x (quot s 32)
        s (bit-xor s x)
        s (bit-and s 0xFFFFFF)
        x (* s 2048)
        s (bit-xor s x)
        s (bit-and s 0xFFFFFF)]
    s))

(defn process-buyer [buyer]
  (loop [s buyer i 0]
    (if (= i 2000)
      s
      (recur (next-secret s) (inc i)))))

(defn solve []
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (remove clojure.string/blank?)
       (map #(Long/parseLong %))
       (pmap process-buyer)
       (reduce + 0)))

(println (solve))
