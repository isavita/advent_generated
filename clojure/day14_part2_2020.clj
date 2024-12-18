
(defn generate-addresses [mask address]
  (let [floating (->> mask
                      (map-indexed vector)
                      (filter (fn [[i c]] (= c \X)))
                      (map (fn [[i _]] (- 35 i))))
        base-address (reduce (fn [addr [i c]]
                              (if (= c \1)
                                (bit-set addr (- 35 i))
                                addr))
                            address
                            (map-indexed vector mask))]
    (->> (range (bit-shift-left 1 (count floating)))
         (map (fn [i]
                (reduce (fn [addr [j pos]]
                          (if (zero? (bit-and i (bit-shift-left 1 j)))
                            (bit-clear addr pos)
                            (bit-set addr pos)))
                        base-address
                        (map-indexed vector floating)))))))

(defn solve []
  (let [lines (-> "input.txt"
                  slurp
                  (clojure.string/split #"\n"))
        mem (atom {})
        mask (atom "")]
    (doseq [line lines]
      (if (clojure.string/starts-with? line "mask = ")
        (reset! mask (subs line 7))
        (let [[_ addr val] (re-matches #"mem\[(\d+)] = (\d+)" line)]
          (when addr
            (let [addresses (generate-addresses @mask (Long/parseLong addr))
                  value (Long/parseLong val)]
              (doseq [a addresses]
                (swap! mem assoc a value)))))))
    (->> @mem vals (reduce + 0))))

(println (solve))
