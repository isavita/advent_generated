(ns decompress
  (:require [clojure.java.io :as io]))

(defn decompress-length [s]
  (loop [i 0 length 0]
    (if (>= i (count s))
      length
      (let [c (nth s i)]
        (if (= c \()
          (let [end (clojure.string/index-of s \) i)
                [chars times] (clojure.string/split (subs s (inc i) end) #"x")
                chars (Integer. chars)
                times (Integer. times)]
            (recur (+ end 1 chars) (+ length (* chars times))))
          (recur (inc i) (inc length)))))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [input (slurp rdr)]
      (println (decompress-length input)))))

(-main)