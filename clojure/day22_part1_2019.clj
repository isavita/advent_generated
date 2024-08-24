(ns shuffle
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn deal-into-new-stack [pos deck-size]
  (mod (- deck-size 1 pos) deck-size))

(defn cut-cards [pos n deck-size]
  (mod (- pos n) deck-size))

(defn deal-with-increment [pos n deck-size]
  (mod (* pos n) deck-size))

(defn shuffle-deck [instructions deck-size]
  (reduce (fn [pos instruction]
            (cond
              (str/starts-with? instruction "deal into new stack") (deal-into-new-stack pos deck-size)
              (str/starts-with? instruction "cut") (cut-cards pos (Integer/parseInt (nth (str/split instruction #" ") 1)) deck-size)
              (str/starts-with? instruction "deal with increment") (deal-with-increment pos (Integer/parseInt (nth (str/split instruction #" ") 3)) deck-size)))
          2019
          instructions))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [instructions (line-seq rdr)
          deck-size 10007
          pos (shuffle-deck instructions deck-size)]
      (println pos))))

(-main)