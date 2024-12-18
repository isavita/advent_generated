
(defn parse-instruction [line]
  (let [[op a b c] (clojure.string/split line #" ")]
    (if (re-matches #"set[ir]" op)
      [(keyword op) (parse-long a) (parse-long b) (parse-long c)]
      [(keyword op) (parse-long a) (parse-long b) (parse-long c)])))

(defn execute-instruction [registers [op a b c]]
  (case op
    :addr (assoc registers c (+ (registers a) (registers b)))
    :addi (assoc registers c (+ (registers a) b))
    :mulr (assoc registers c (* (registers a) (registers b)))
    :muli (assoc registers c (* (registers a) b))
    :banr (assoc registers c (bit-and (registers a) (registers b)))
    :bani (assoc registers c (bit-and (registers a) b))
    :borr (assoc registers c (bit-or (registers a) (registers b)))
    :bori (assoc registers c (bit-or (registers a) b))
    :setr (assoc registers c (registers a))
    :seti (assoc registers c a)
    :gtir (assoc registers c (if (> a (registers b)) 1 0))
    :gtri (assoc registers c (if (> (registers a) b) 1 0))
    :gtrr (assoc registers c (if (> (registers a) (registers b)) 1 0))
    :eqir (assoc registers c (if (= a (registers b)) 1 0))
    :eqri (assoc registers c (if (= (registers a) b) 1 0))
    :eqrr (assoc registers c (if (= (registers a) (registers b)) 1 0))))

(defn run-program [instructions ip-reg]
  (loop [registers (vec (repeat 6 0))
         ip 0]
    (if (or (>= ip (count instructions)) (< ip 0))
      (first registers)
      (let [instruction (nth instructions ip)
            registers (assoc registers ip-reg ip)
            registers (execute-instruction registers instruction)
            ip (inc (registers ip-reg))]
        (recur registers ip)))))

(defn parse-input [input]
  (let [[ip-line & instruction-lines] (clojure.string/split-lines input)
        ip-reg (parse-long (re-find #"\d+" ip-line))]
    {:ip-reg ip-reg
     :instructions (map parse-instruction instruction-lines)}))

(defn solve []
  (let [input (slurp "input.txt")
        {:keys [ip-reg instructions]} (parse-input input)]
    (run-program instructions ip-reg)))

(println (solve))
