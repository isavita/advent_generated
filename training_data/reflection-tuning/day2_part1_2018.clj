(ns day2.core
  (:require [clojure.java.io :as io]))

(defn has-n-of-any-letter? [freq n]
  (some #(= n (val %)) freq))

(defn count-doubles-and-triples [s]
  (let [freq (frequencies s)]
    [(if (has-n-of-any-letter? freq 2) 1 0)
     (if (has-n-of-any-letter? freq 3) 1 0)]))

(defn calculate-checksum [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [[doubles triples] (reduce (fn [[d t] line]
                                      (let [[double triple] (count-doubles-and-triples line)]
                                        [(+ d double) (+ t triple)]))
                                    [0 0]
                                    (line-seq rdr))]
      (* doubles triples))))

(defn -main []
  (println (calculate-checksum "input.txt")))

(-main)
