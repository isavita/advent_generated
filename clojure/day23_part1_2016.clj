
(defn read-instructions [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))

(defn get-value [s registers]
  (if-let [val (parse-long s)]
    val
    (get registers s)))

(defn toggle-instruction [instr]
  (let [parts (clojure.string/split instr #" ")]
    (case (first parts)
      "inc" (str "dec " (second parts))
      ("dec" "tgl") (str "inc " (second parts))
      "jnz" (str "cpy " (second parts) " " (nth parts 2))
      "cpy" (str "jnz " (second parts) " " (nth parts 2))
      instr)))

(defn execute-instructions [instructions registers]
  (loop [pc 0
         instrs (vec instructions)
         regs registers]
    (if (>= pc (count instrs))
      regs
      (let [fields (clojure.string/split (nth instrs pc) #" ")
            op (first fields)]
        (case op
          "cpy" (let [x (get-value (second fields) regs)
                      y (nth fields 2)]
                  (if (contains? regs y)
                    (recur (inc pc) instrs (assoc regs y x))
                    (recur (inc pc) instrs regs)))
          "inc" (let [x (second fields)]
                  (if (contains? regs x)
                    (recur (inc pc) instrs (update regs x inc))
                    (recur (inc pc) instrs regs)))
          "dec" (let [x (second fields)]
                  (if (contains? regs x)
                    (recur (inc pc) instrs (update regs x dec))
                    (recur (inc pc) instrs regs)))
          "jnz" (let [x (get-value (second fields) regs)
                      y (get-value (nth fields 2) regs)]
                  (if (not= x 0)
                    (recur (+ pc y) instrs regs)
                    (recur (inc pc) instrs regs)))
          "tgl" (let [x (get-value (second fields) regs)
                      tgt (+ pc x)]
                  (if (and (>= tgt 0) (< tgt (count instrs)))
                    (recur (inc pc) (assoc instrs tgt (toggle-instruction (nth instrs tgt))) regs)
                    (recur (inc pc) instrs regs)))
          (recur (inc pc) instrs regs))))))

(let [instructions (read-instructions "input.txt")
      registers {"a" 7, "b" 0, "c" 0, "d" 0}]
  (println (get (execute-instructions instructions registers) "a")))
