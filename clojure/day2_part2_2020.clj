(ns day2
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ min max char password] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :char (first char)
     :password password}))

(defn valid-password? [policy]
  (let [char-count (count (filter #(= (:char policy) %) (:password policy)))]
    (<= (:min policy) char-count (:max policy))))

(defn valid-password-new-policy? [policy]
  (let [password (:password policy)
        char (:char policy)
        pos1 (= char (nth password (dec (:min policy))))
        pos2 (= char (nth password (dec (:max policy))))]
    (or (and pos1 (not pos2)) (and pos2 (not pos1)))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [lines (line-seq rdr)
          policies (map parse-line lines)
          valid-count (count (filter valid-password? policies))
          valid-count-new (count (filter valid-password-new-policy? policies))]
      (println "Part 1:" valid-count)
      (println "Part 2:" valid-count-new))))

(-main)