(ns passphrase-validator
  (:require [clojure.string :as str]))

(defn valid-passphrase? [phrase]
  (let [words (str/split phrase #"\s+")]
    (= (count words) (count (distinct words)))))

(defn valid-passphrase-anagram? [phrase]
  (let [words (str/split phrase #"\s+")
        sorted-words (map #(apply str (sort %)) words)]
    (= (count sorted-words) (count (distinct sorted-words)))))

(defn count-valid-passphrases [validator-fn]
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (count (filter validator-fn (line-seq rdr)))))

(defn -main []
  (let [part1-count (count-valid-passphrases valid-passphrase?)
        part2-count (count-valid-passphrases valid-passphrase-anagram?)]
    (println "Part 1 valid passphrases:" part1-count)
    (println "Part 2 valid passphrases:" part2-count)))

(-main)
