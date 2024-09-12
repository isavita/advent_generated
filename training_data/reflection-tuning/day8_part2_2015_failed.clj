(ns day8.core
  (:require [clojure.string :as str]))

(defn unescape-string [s]
  (-> s
      (str/replace #"\\\\|\\\"" "_")
      (str/replace #"\\x[0-9a-f]{2}" "_")))

(defn count-code-chars [s]
  (count s))

(defn count-memory-chars [s]
  (-> s
      (subs 1 (dec (count s)))
      unescape-string
      count))

(defn encode-string [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (->> (str "\"") (str "\""))))

(defn count-encoded-chars [s]
  (count (encode-string s)))

(defn solve-part1 [input]
  (let [strings (str/split-lines input)]
    (- (reduce + (map count-code-chars strings))
       (reduce + (map count-memory-chars strings)))))

(defn solve-part2 [input]
  (let [strings (str/split-lines input)]
    (- (reduce + (map count-encoded-chars strings))
       (reduce + (map count-code-chars strings)))))

(defn -main [& args]
  (let [input (slurp "resources/input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))
