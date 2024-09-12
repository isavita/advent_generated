(require '[clojure.string :as str])

(defn most-common-char [chars]
  (first (apply max-key val (frequencies chars))))

(defn decode-message [input]
  (->> input
       str/split-lines
       (apply map vector)
       (map most-common-char)
       (apply str)))

(println (decode-message (slurp "input.txt")))
