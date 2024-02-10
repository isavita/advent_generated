
(defn calculate-paper [l w h]
  (let [sides [( * l w) ( * w h) ( * h l)]
        smallest-side (apply min sides)
        total-area (+ (* 2 l w) (* 2 w h) (* 2 h l))]
    (+ total-area smallest-side)))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [input (line-seq rdr)
          total-paper (reduce + (map #(let [[l w h] (map read-string (clojure.string/split % #"x"))]
                                      (calculate-paper l w h)) input))]
      (println total-paper))))

(-main)
