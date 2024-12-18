
(defn solve []
  (let [data (slurp "input.txt")
        width 25
        height 6
        layer-size (* width height)
        layers (partition layer-size data)
        final-image (reduce (fn [img layer]
                             (map (fn [p i] (if (= (nth img i) \2) p (nth img i))) layer (range)))
                           (repeat layer-size \2)
                           layers)]
    (println "Decoded image:")
    (doseq [row (partition width final-image)]
      (println (apply str (map #(if (= % \0) " " (if (= % \1) "#" "")) row))))))

(solve)
