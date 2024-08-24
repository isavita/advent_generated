(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn binary-to-int [s zero-char]
  (reduce (fn [acc c]
            (+ (* acc 2) (if (= c zero-char) 0 1)))
          0 s))

(defn parse-seat [s]
  (let [row (binary-to-int (subs s 0 7) \F)
        column (binary-to-int (subs s 7) \L)]
    (+ (* row 8) column)))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [max-seat-id (reduce max (map parse-seat (line-seq r)))]
      (println max-seat-id))))

(-main)