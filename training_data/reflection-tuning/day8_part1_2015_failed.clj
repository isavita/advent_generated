(ns day8.matchsticks
  (:require [clojure.string :as str]))

(defn count-code-chars [s]
  (count s))

(defn count-memory-chars [s]
  (-> s
      (subs 1 (dec (count s)))  ; Remove surrounding quotes
      (str/replace #"\\\\|\\\"" "S")  ; Replace \\ and \" with a single character
      (str/replace #"\\x[0-9a-fA-F]{2}" "H")  ; Replace \x hex escapes with a single character
      count))

(defn process-input [input]
  (let [strings (str/split-lines input)
        code-chars (reduce + (map count-code-chars strings))
        memory-chars (reduce + (map count-memory-chars strings))]
    (- code-chars memory-chars)))

(defn solve [input]
  (process-input input))

; Example usage:
(def sample-input "\"\"
\"abc\"
\"aaa\\\"aaa\"
\"\\x27\"")

(println (solve sample-input))  ; Should output 12
