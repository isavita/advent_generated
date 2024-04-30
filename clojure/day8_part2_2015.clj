(ns day8
  (:require [clojure.string :as str]))

(defn count-code-chars [s]
  (count s))

(defn count-memory-chars [s]
  (let [s (subs s 1 (dec (count s)))]
    (loop [s s, count 0]
      (cond
        (empty? s) count
        (= (first s) \\) (recur (subs s 2) (inc count))
        :else (recur (subs s 1) (inc count))))))

(defn encode-string [s]
  (str "\"" (str/replace (str/replace s "\\" "\\\\") "\"" "\\\"") "\""))

(defn count-encoded-chars [s]
  (count (encode-string s)))

(defn solve []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [lines (line-seq rdr)
          code-chars (apply + (map count-code-chars lines))
          memory-chars (apply + (map count-memory-chars lines))
          encoded-chars (apply + (map count-encoded-chars lines))]
      (println "Part 1:" (- code-chars memory-chars))
      (println "Part 2:" (- encoded-chars code-chars)))))

(solve)