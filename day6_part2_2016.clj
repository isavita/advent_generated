
(defn most-common [input]
  (apply str (map #(->> % frequencies (sort-by val >) first key) (apply map vector input))))

(defn least-common [input]
  (apply str (map #(->> % frequencies (sort-by val) first key) (apply map vector input))))

(defn read-input []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (doall (line-seq rdr))))

(def input (read-input))

(println (most-common input))
(println (least-common input))
