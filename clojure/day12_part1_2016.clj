(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn execute-instructions [instructions registers]
  (loop [i 0 registers registers]
    (if (>= i (count instructions))
      registers
      (let [parts (str/split (nth instructions i) #" ")
            op (first parts)
            val (if (re-matches #"\d+" (second parts))
                   (Integer/parseInt (second parts))
                   (registers (second parts)))]
        (case op
          "cpy" (recur (inc i) (assoc registers (nth parts 2) val))
          "inc" (recur (inc i) (update registers (second parts) inc))
          "dec" (recur (inc i) (update registers (second parts) dec))
          "jnz" (if (not= val 0)
                   (recur (+ i (Integer/parseInt (nth parts 2))) registers)
                   (recur (inc i) registers)))))))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [instructions (line-seq r)
          registers {"a" 0 "b" 0 "c" 0 "d" 0}]
      (println (get (execute-instructions instructions registers) "a")))))

(-main)