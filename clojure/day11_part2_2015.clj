
(defn valid-password? [password]
  (let [forbidden-chars #{"i" "o" "l"}
        straight (re-find #"(abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn)" password)
        pairs (count (re-seq #"(.)\1" password))]
    (and straight
         (not (some #(contains? forbidden-chars %) password))
         (>= pairs 2))))

(defn increment-password [password]
  (loop [pw (reverse password)
         carry? true
         result ""]
    (if (empty? pw)
      (if carry?
        (str "a" result)
        result)
      (let [c (first pw)
            rest (next pw)
            next-c (if (and carry? (= c \z))
                     \a
                     (if carry?
                       (char (inc (int c)))
                       c)
                     )
            next-carry? (and carry? (= next-c \a))]
        (recur rest next-carry? (str next-c result))))))

(defn find-next-password [password]
  (loop [pw password]
    (let [next-pw (increment-password pw)]
      (if (valid-password? next-pw)
        next-pw
        (recur next-pw)))))

(def input (slurp "input.txt"))
(println (find-next-password input))
