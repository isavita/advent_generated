
(require '[clojure.string :as str])

(def instructions
  {"addr" (fn [r a b] (+ (r a) (r b)))
   "addi" (fn [r a b] (+ (r a) b))
   "mulr" (fn [r a b] (* (r a) (r b)))
   "muli" (fn [r a b] (* (r a) b))
   "banr" (fn [r a b] (bit-and (r a) (r b)))
   "bani" (fn [r a b] (bit-and (r a) b))
   "borr" (fn [r a b] (bit-or (r a) (r b)))
   "bori" (fn [r a b] (bit-or (r a) b))
   "setr" (fn [r a b] (r a))
   "seti" (fn [r a b] a)
   "gtir" (fn [r a b] (if (> a (r b)) 1 0))
   "gtri" (fn [r a b] (if (> (r a) b) 1 0))
   "gtrr" (fn [r a b] (if (> (r a) (r b)) 1 0))
   "eqir" (fn [r a b] (if (= a (r b)) 1 0))
   "eqri" (fn [r a b] (if (= (r a) b) 1 0))
   "eqrr" (fn [r a b] (if (= (r a) (r b)) 1 0))})

(defn parse-int [s]
  (Integer/parseInt s))

(defn load-program [lines]
  (let [ip-line (first (filter #(str/starts-with? % "#ip") lines))
        ip-register (parse-int (re-find #"\d+" ip-line))
        program (remove #(str/starts-with? % "#ip") lines)
        program (map #(let [[op a b c] (str/split % #" ")
                             a (parse-int a)
                             b (parse-int b)
                             c (parse-int c)]
                         [(get instructions op) a b c]) program)]
    [ip-register program]))

(defn run-program [ip-register program registers max-cycles]
  (loop [ip 0
         registers (vec registers)
         cycles 0]
    (if (or (>= ip (count program)) (< ip 0) (and (> max-cycles 0) (>= cycles max-cycles)))
      registers
      (let [[op a b c] (nth program ip)
            registers (assoc registers ip-register ip)
            new-registers (assoc registers c (op registers a b))
            new-ip (inc (get new-registers ip-register))]
        (recur new-ip new-registers (inc cycles))))))

(defn sum-of-divisors [n]
  (reduce + (filter (fn [i] (= 0 (rem n i))) (range 1 (inc n)))))

(defn solve []
  (let [lines (-> "input.txt" slurp str/split-lines vec)
        [ip-register program] (load-program lines)
        registers (vec [1 0 0 0 0 0])
        final-registers (run-program ip-register program registers 1000)
        n (apply max final-registers)]
    (sum-of-divisors n)))

(defn main []
  (println (solve)))

(main)
