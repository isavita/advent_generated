(ns sue
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def mfcsam {"children" 3, "cats" 7, "samoyeds" 2, "pomeranians" 3,
             "akitas" 0, "vizslas" 0, "goldfish" 5, "trees" 3,
             "cars" 2, "perfumes" 1})

(with-open [r (io/reader "input.txt")]
  (doseq [line (line-seq r)]
    (let [parts (str/split line #"\s+")
          sue-number (subs (nth parts 1) 0 (dec (count (nth parts 1))))
          matches (loop [i 2]
                    (if (>= i (count parts))
                      true
                      (let [item (subs (nth parts i) 0 (dec (count (nth parts i))))
                            count-str (subs (nth parts (inc i)) 0 (dec (count (nth parts (inc i)))))
                            count (if (re-matches #"\d+" count-str)
                                    (Integer/parseInt count-str)
                                    0)]
                        (if (= (mfcsam item) count)
                          (recur (+ i 2))
                          false))))]
      (when matches
        (println sue-number)
        (System/exit 0)))))