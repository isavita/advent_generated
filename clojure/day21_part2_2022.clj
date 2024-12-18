
(defn parse-monkey [s index]
  (let [[name rest] (clojure.string/split s #": ")
        num (try (Integer/parseInt rest) (catch Exception _ nil))]
    (if num
      (assoc index name {:val num :has-val true})
      (let [[left op right] (clojure.string/split rest #" ")]
        (assoc index name {:left left :op op :right right})))))

(defn parse []
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (reduce (fn [index line] (parse-monkey line index)) {})))

(defn solve [monkeys name]
  (let [m (get monkeys name)]
    (if (:has-val m)
      [(:val m) true]
      (let [{:keys [left op right]} m]
        (if (and left right)
          (let [[l-val l-ok] (solve monkeys left)
                [r-val r-ok] (solve monkeys right)]
            (if (and l-ok r-ok)
              (case op
                "+" [(+ l-val r-val) true]
                "-" [(- l-val r-val) true]
                "*" [(* l-val r-val) true]
                "/" [(quot l-val r-val) true]
                "==" [(if (= l-val r-val) 0 1) true])
              [nil false]))
          [nil false])))))

(defn expect [monkeys name x]
  (if (= name "humn")
    x
    (let [m (get monkeys name)
          [l-val l-ok] (solve monkeys (:left m))
          [r-val r-ok] (solve monkeys (:right m))
          op (:op m)]
      (cond
        (not l-ok)
        (case op
          "+" (expect monkeys (:left m) (- x r-val))
          "-" (expect monkeys (:left m) (+ x r-val))
          "*" (expect monkeys (:left m) (quot x r-val))
          "/" (expect monkeys (:left m) (* x r-val))
          "==" (expect monkeys (:left m) r-val))
        (not r-ok)
        (case op
          "+" (expect monkeys (:right m) (- x l-val))
          "-" (expect monkeys (:right m) (- l-val x))
          "*" (expect monkeys (:right m) (quot x l-val))
          "/" (expect monkeys (:right m) (quot l-val x))
          "==" (expect monkeys (:right m) l-val))
        :else (throw (Exception. "impossible"))))))

(let [monkeys (-> (parse)
                  (assoc-in ["humn" :has-val] false)
                  (assoc-in ["root" :op] "=="))]
  (println (expect monkeys "root" 0)))
