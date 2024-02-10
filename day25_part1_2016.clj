
(ns solution
  (:require [clojure.string :as str]))

(defn get-value [s registers]
  (if-let [val (try (Integer/parseInt s) (catch Exception e (get registers s)))]
    val
    0))

(defn produces-clock-signal [a instructions]
  (loop [registers {"a" a, "b" 0, "c" 0, "d" 0}
         last-output 0
         output-count 0
         i 0]
    (if (< i (count instructions))
      (let [parts (str/split (nth instructions i) #" ")
            op (first parts)]
        (cond
          (= op "cpy") (recur (assoc registers (last parts) (get-value (second parts) registers)) last-output output-count (inc i))
          (= op "inc") (recur (update registers (last parts) inc) last-output output-count (inc i))
          (= op "dec") (recur (update registers (last parts) dec) last-output output-count (inc i))
          (= op "jnz") (if (not= 0 (get-value (second parts) registers))
                        (recur registers last-output output-count (+ i (Integer/parseInt (last parts))))
                        (recur registers last-output output-count (inc i)))
          (= op "out") (let [val (get-value (second parts) registers)]
                         (if (not (or (= val 0) (= val 1)))
                           false
                           (if (and (> output-count 0) (= val last-output))
                             false
                             (if (>= output-count 50)
                               true
                               (recur registers val (inc output-count) (inc i))))))))
      false)))

(defn -main []
  (with-open [file (clojure.java.io/reader "input.txt")]
    (let [instructions (doall (line-seq file))]
      (loop [a 1]
        (if (produces-clock-signal a instructions)
          (do (println a) (System/exit 0))
          (recur (inc a)))))))

(-main)
