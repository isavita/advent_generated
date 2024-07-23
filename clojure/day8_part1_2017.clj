
(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))

(defn parse-line [line]
  (let [[reg op amount _ cond-reg cond-op cond-val] (clojure.string/split line #"\s+")]
    [(keyword reg) op (Integer/parseInt amount) (keyword cond-reg) cond-op (Integer/parseInt cond-val)]))

(defn evaluate-condition [registers cond-reg cond-op cond-val]
  (case cond-op
    ">"  (> (get registers cond-reg 0) cond-val)
    ">=" (>= (get registers cond-reg 0) cond-val)
    "<"  (< (get registers cond-reg 0) cond-val)
    "<=" (<= (get registers cond-reg 0) cond-val)
    "==" (== (get registers cond-reg 0) cond-val)
    "!=" (not= (get registers cond-reg 0) cond-val)))

(defn update-register [registers reg op amount]
  (update registers reg (fnil (if (= op "inc") + -) 0) amount))

(defn process-instructions [lines]
  (reduce (fn [registers line]
            (let [[reg op amount cond-reg cond-op cond-val] (parse-line line)]
              (if (evaluate-condition registers cond-reg cond-op cond-val)
                (update-register registers reg op amount)
                registers)))
          {}
          lines))

(defn max-register-value [registers]
  (apply max (vals registers)))

(defn -main []
  (let [lines (read-input "input.txt")
        registers (process-instructions lines)]
    (println (max-register-value registers))))

(-main)
