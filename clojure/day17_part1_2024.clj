
(defn parse-input [lines]
  (loop [lines lines
         A 0
         B 0
         C 0
         program []]
    (if (empty? lines)
      {:A A :B B :C C :program program}
      (let [line (first lines)
            line (clojure.string/trim line)]
        (if (empty? line)
          (recur (rest lines) A B C program)
          (cond
            (clojure.string/starts-with? line "Register A:")
            (recur (rest lines) (Integer/parseInt (clojure.string/trim (second (clojure.string/split line #":")))) B C program)
            (clojure.string/starts-with? line "Register B:")
            (recur (rest lines) A (Integer/parseInt (clojure.string/trim (second (clojure.string/split line #":")))) C program)
            (clojure.string/starts-with? line "Register C:")
            (recur (rest lines) A B (Integer/parseInt (clojure.string/trim (second (clojure.string/split line #":")))) program)
            (clojure.string/starts-with? line "Program:")
            (recur (rest lines) A B C (map #(Integer/parseInt (clojure.string/trim %)) (clojure.string/split (clojure.string/trim (second (clojure.string/split line #":"))) #",")))
            :else (recur (rest lines) A B C program)))))))

(defn get-combo-val [state op]
  (case op
    (1 2 3) op
    4 (:A state)
    5 (:B state)
    6 (:C state)
    (throw (Exception. "invalid combo operand"))))

(defn execute-program [{:keys [A B C program] :as state}]
  (loop [ip 0
         A A
         B B
         C C
         output-vals []]
    (if (or (>= ip (count program)) (>= (+ ip 1) (count program)))
      (clojure.string/join "," output-vals)
      (let [opcode (nth program ip)
            operand (nth program (inc ip))]
        (case opcode
          0 (let [den (get-combo-val {:A A :B B :C C} operand)
                  new-A (if (zero? den) 0 (quot A (int (Math/pow 2 den))))]
              (recur (+ ip 2) new-A B C output-vals))
          1 (recur (+ ip 2) A (bit-xor B operand) C output-vals)
          2 (recur (+ ip 2) A (mod (get-combo-val {:A A :B B :C C} operand) 8) C output-vals)
          3 (if (not (zero? A))
              (recur operand A B C output-vals)
              (recur (+ ip 2) A B C output-vals))
          4 (recur (+ ip 2) A (bit-xor B C) C output-vals)
          5 (recur (+ ip 2) A B C (conj output-vals (str (mod (get-combo-val {:A A :B B :C C} operand) 8))))
          6 (let [den (get-combo-val {:A A :B B :C C} operand)
                  new-B (quot A (int (Math/pow 2 den)))]
              (recur (+ ip 2) A new-B C output-vals))
          7 (let [den (get-combo-val {:A A :B B :C C} operand)
                  new-C (quot A (int (Math/pow 2 den)))]
              (recur (+ ip 2) A B new-C output-vals))
          (recur (+ ip 2) A B C output-vals))))))

(defn solve []
  (let [lines (-> "input.txt" slurp clojure.string/split-lines)
        initial-state (parse-input lines)]
    (execute-program initial-state)))

(println (solve))
