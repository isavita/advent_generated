
(defn knot-hash [input]
  (let [lengths (concat (map int (.getBytes input)) [17 31 73 47 23])
        list (vec (range 256))]
    (loop [list list
           current-position 0
           skip-size 0
           round 0]
      (if (= round 64)
        (->> (partition 16 list)
             (map #(reduce bit-xor 0 %))
             (map (partial format "%02x"))
             (apply str))
        (let [[new-list new-current-position new-skip-size]
              (reduce (fn [[list current-position skip-size] length]
                        (let [sublist (take length (drop current-position (cycle list)))
                              reversed-sublist (reverse sublist)
                              new-list (loop [l list
                                              i 0]
                                         (if (= i length)
                                           l
                                           (let [idx (rem (+ current-position i) 256)
                                                 val (nth reversed-sublist i)]
                                             (recur (assoc l idx val) (inc i)))))]
                          [(vec new-list)
                           (rem (+ current-position length skip-size) 256)
                           (inc skip-size)]))
                      [list current-position skip-size]
                      lengths)]
          (recur new-list new-current-position new-skip-size (inc round)))))))

(defn solve []
  (let [input (slurp "input.txt")]
    (println (knot-hash input))))

(solve)
