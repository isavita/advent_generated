
(require '[clojure.string :as str])

(def DECK_SIZE 119315717514047N)
(def ITERATIONS 101741582076661N)
(def TARGET_POSITION 2020N)

(defn mod-inverse [a m]
  (loop [t 0N new-t 1N r m new-r a]
    (if (zero? new-r)
      (cond-> t (neg? t) (+ m))
      (let [q (quot r new-r)]
        (recur new-t (- t (* q new-t)) new-r (- r (* q new-r)))))))

(defn process-instruction [line]
  (cond
    (= line "deal into new stack") [-1N -1N]
    (str/starts-with? line "cut") [1N (- (bigint (last (str/split line #" "))))]
    (str/starts-with? line "deal with increment") [(bigint (last (str/split line #" "))) 0N]))

(defn combine [[a1 b1] [a2 b2]]
  [(mod (* a1 a2) DECK_SIZE)
   (mod (+ (* a2 b1) b2) DECK_SIZE)])

(defn matrix-exp [base exp]
  (loop [result [1N 0N] base base exp exp]
    (if (zero? exp)
      result
      (recur (if (odd? exp) (combine result base) result)
             (combine base base)
             (quot exp 2)))))

(let [lines (str/split-lines (str/trim (slurp "input.txt")))
      transform (reduce combine [1N 0N] (map process-instruction lines))
      [a b] (matrix-exp transform ITERATIONS)
      num (mod (- TARGET_POSITION b) DECK_SIZE)
      den (mod-inverse a DECK_SIZE)]
  (println (mod (* num den) DECK_SIZE)))
