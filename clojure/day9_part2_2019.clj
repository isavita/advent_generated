
(defn parse-program [s]
  (->> (clojure.string/split s #",")
       (map #(Integer/parseInt %))
       (into {} (map-indexed vector))))

(defn get-param [memory modes ip offset relative-base]
  (let [mode (or (some-> modes (nth (- (count modes) offset) nil) str Integer/parseInt) 0)
        param (get memory (+ ip offset))]
    (case mode
      0 (get memory param 0)
      1 param
      2 (get memory (+ relative-base param) 0)
      (throw (Exception. "unknown parameter mode")))))

(defn set-param [memory modes ip offset relative-base value]
  (let [mode (or (some-> modes (nth (- (count modes) offset) nil) str Integer/parseInt) 0)
        param (get memory (+ ip offset))]
    (case mode
      0 (assoc memory param value)
      2 (assoc memory (+ relative-base param) value)
      (throw (Exception. "unknown parameter mode")))))

(defn run-intcode [memory input]
  (loop [memory memory ip 0 relative-base 0 output nil]
    (let [opcode (mod (get memory ip 0) 100)
          modes (str (quot (get memory ip 0) 100))]
      (case opcode
        1 (recur (set-param memory modes ip 3 relative-base (+ (get-param memory modes ip 1 relative-base) (get-param memory modes ip 2 relative-base))) (+ ip 4) relative-base output)
        2 (recur (set-param memory modes ip 3 relative-base (* (get-param memory modes ip 1 relative-base) (get-param memory modes ip 2 relative-base))) (+ ip 4) relative-base output)
        3 (recur (set-param memory modes ip 1 relative-base input) (+ ip 2) relative-base output)
        4 (recur memory (+ ip 2) relative-base (get-param memory modes ip 1 relative-base))
        5 (recur memory (if (not= (get-param memory modes ip 1 relative-base) 0) (get-param memory modes ip 2 relative-base) (+ ip 3)) relative-base output)
        6 (recur memory (if (= (get-param memory modes ip 1 relative-base) 0) (get-param memory modes ip 2 relative-base) (+ ip 3)) relative-base output)
        7 (recur (set-param memory modes ip 3 relative-base (if (< (get-param memory modes ip 1 relative-base) (get-param memory modes ip 2 relative-base)) 1 0)) (+ ip 4) relative-base output)
        8 (recur (set-param memory modes ip 3 relative-base (if (= (get-param memory modes ip 1 relative-base) (get-param memory modes ip 2 relative-base)) 1 0)) (+ ip 4) relative-base output)
        9 (recur memory (+ ip 2) (+ relative-base (get-param memory modes ip 1 relative-base)) output)
        99 output
        (throw (Exception. (str "unknown opcode: " opcode)))))))

(defn solve []
  (let [program (-> (slurp "input.txt") parse-program)]
    (run-intcode program 2)))

(println (solve))
