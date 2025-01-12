
(defn spin [programs x]
  (let [n (count programs)]
    (vec (for [i (range n)]
           (nth programs (mod (- i x) n))))))

(defn exchange [programs a b]
  (let [temp (nth programs a)]
    (-> programs
        (assoc a (nth programs b))
        (assoc b temp))))

(defn partner [programs a b]
  (let [index-a (.indexOf programs a)
        index-b (.indexOf programs b)]
    (exchange programs index-a index-b)))

(defn parse-move [move]
  (let [op (first move)
        rest (subs move 1)]
    (case op
      \s (let [x (Integer/parseInt rest)]
           (fn [programs] (spin programs x)))
      \x (let [[a b] (map #(Integer/parseInt %) (clojure.string/split rest #"/"))]
           (fn [programs] (exchange programs a b)))
      \p (let [[a b] (clojure.string/split rest #"/")]
           (fn [programs] (partner programs (first a) (first b)))))))

(defn apply-moves [programs moves]
  (reduce (fn [progs move] (move progs)) programs moves))

(defn solve []
  (let [moves (-> (slurp "input.txt")
                  (clojure.string/trim)
                  (clojure.string/split #"," )
                  (->> (map parse-move)))
        initial (vec "abcdefghijklmnop")
        ]
    (loop [programs initial
           seen {}
           i 0]
      (if (seen programs)
        (let [cycle-len i
              remaining (mod 1000000000 cycle-len)]
          (loop [progs initial
                 j 0]
            (if (= j remaining)
              (apply str progs)
              (recur (apply-moves progs moves) (inc j)))))
        (recur (apply-moves programs moves)
               (assoc seen programs true)
               (inc i))))))

(println (solve))
