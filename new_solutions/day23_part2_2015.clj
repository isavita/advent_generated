(ns day23
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[op arg1 arg2] (str/split line #"\s+")]
    (cond
      (= op "hlf") [:hlf (keyword arg1)]
      (= op "tpl") [:tpl (keyword arg1)]
      (= op "inc") [:inc (keyword arg1)]
      (= op "jmp") [:jmp (Integer/parseInt arg1)]
      (= op "jie") [:jie (keyword (subs arg1 0 (dec (count arg1)))) (Integer/parseInt arg2)]
      (= op "jio") [:jio (keyword (subs arg1 0 (dec (count arg1)))) (Integer/parseInt arg2)])))

(defn execute-instructions [instructions registers]
  (loop [registers registers
         pc 0]
    (if (>= pc (count instructions))
      registers
      (let [[op arg1 arg2] (nth instructions pc)]
        (case op
          :hlf (recur (update registers arg1 #(quot % 2)) (inc pc))
          :tpl (recur (update registers arg1 #(* % 3)) (inc pc))
          :inc (recur (update registers arg1 inc) (inc pc))
          :jmp (recur registers (+ pc arg1))
          :jie (if (even? (get registers arg1))
                 (recur registers (+ pc arg2))
                 (recur registers (inc pc)))
          :jio (if (= 1 (get registers arg1))
                 (recur registers (+ pc arg2))
                 (recur registers (inc pc))))))))

(defn -main []
  (let [instructions (map parse-instruction (line-seq (clojure.java.io/reader "input.txt")))]
    (let [registers (execute-instructions instructions {:a 0 :b 0})]
      (println "Part 1:" (:b registers)))
    (let [registers (execute-instructions instructions {:a 1 :b 0})]
      (println "Part 2:" (:b registers)))))

(-main)