
(def input (slurp "input.txt"))

(defn decode-string [s]
  (let [escaped (clojure.string/replace s #"\\x[0-9a-f]{2}|\\\"|\\\\" "X")]
    (- (count s) (- (count escaped) 2))))

(defn solve []
  (->> (clojure.string/split-lines input)
       (map decode-string)
       (reduce +)))

(prn (solve))
