(ns circuit-solver
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[_ source dest] (re-find #"(.+) -> (\w+)" line)]
    [dest source]))

(defn parse-input [filename]
  (into {} (map parse-instruction (str/split-lines (slurp filename)))))

(def circuit (parse-input "input.txt"))

(defn evaluate-signal [wire]
  (let [memo (memoize
              (fn [w]
                (if-let [source (get circuit w)]
                  (let [parts (str/split source #" ")]
                    (cond
                      (= (count parts) 1)
                      (if (re-matches #"\d+" (first parts))
                        (Integer/parseInt (first parts))
                        (recur (first parts)))

                      (= (count parts) 2)
                      (bit-not (evaluate-signal (second parts)))

                      (= (count parts) 3)
                      (let [[a op b] parts
                            a (if (re-matches #"\d+" a) (Integer/parseInt a) (evaluate-signal a))
                            b (if (re-matches #"\d+" b) (Integer/parseInt b) (evaluate-signal b))]
                        (case op
                          "AND" (bit-and a b)
                          "OR" (bit-or a b)
                          "LSHIFT" (bit-shift-left a b)
                          "RSHIFT" (bit-shift-right a b)))))
                  (Integer/parseInt w))))]
    (mod (memo wire) 65536)))

(defn -main []
  (println (evaluate-signal "a")))

(-main)
