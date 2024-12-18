
(defn get-value [program pos mode]
  (if (= mode 0)
    (program (program pos))
    (program pos)))

(defn run-program [program input]
  (loop [program program i 0 output 0]
    (let [opcode (mod (program i) 100)
          modes (quot (program i) 100)
          param1-mode (mod modes 10)
          param2-mode (mod (quot modes 10) 10)]
      (case opcode
        1 (let [p1 (get-value program (inc i) param1-mode)
                p2 (get-value program (+ i 2) param2-mode)
                p3 (program (+ i 3))]
            (recur (assoc program p3 (+ p1 p2)) (+ i 4) output))
        2 (let [p1 (get-value program (inc i) param1-mode)
                p2 (get-value program (+ i 2) param2-mode)
                p3 (program (+ i 3))]
            (recur (assoc program p3 (* p1 p2)) (+ i 4) output))
        3 (recur (assoc program (program (inc i)) input) (+ i 2) output)
        4 (let [out (get-value program (inc i) param1-mode)]
            (println out)
            (recur program (+ i 2) out))
        5 (let [p1 (get-value program (inc i) param1-mode)
                p2 (get-value program (+ i 2) param2-mode)]
            (if (not= p1 0)
              (recur program p2 output)
              (recur program (+ i 3) output)))
        6 (let [p1 (get-value program (inc i) param1-mode)
                p2 (get-value program (+ i 2) param2-mode)]
            (if (= p1 0)
              (recur program p2 output)
              (recur program (+ i 3) output)))
        7 (let [p1 (get-value program (inc i) param1-mode)
                p2 (get-value program (+ i 2) param2-mode)
                p3 (program (+ i 3))]
            (recur (assoc program p3 (if (< p1 p2) 1 0)) (+ i 4) output))
        8 (let [p1 (get-value program (inc i) param1-mode)
                p2 (get-value program (+ i 2) param2-mode)
                p3 (program (+ i 3))]
            (recur (assoc program p3 (if (= p1 p2) 1 0)) (+ i 4) output))
        99 nil
        (throw (Exception. "Invalid opcode"))))))

(defn parse-program [s]
  (->> (clojure.string/split s #",")
       (map #(Integer/parseInt %))
       (vec)))

(defn solve []
  (let [program-str (slurp "input.txt")
        program (parse-program program-str)]
    (run-program program 5)))

(solve)
