(defn read-input []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (Integer/parseInt (first (line-seq rdr)))))

(defn find-winner [n]
  (let [highest-power-of-two (loop [x 1]
                               (if (<= (* x 3) n)
                                 (recur (* x 3))
                                 x))]
    (if (<= highest-power-of-two n)
      (- n highest-power-of-two)
      (+ (* 2 (- n highest-power-of-two)) 1))))

(println (find-winner (read-input)))