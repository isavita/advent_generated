
(defn solve []
  (let [input (slurp "input.txt")
        result (loop [s input
                      score 0
                      depth 0
                      in-garbage false
                      cancel-next false
                      garbage-count 0]
                 (if (empty? s)
                   garbage-count
                   (let [ch (first s)]
                     (if cancel-next
                       (recur (rest s) score depth in-garbage false garbage-count)
                       (if in-garbage
                         (cond
                           (= ch \!) (recur (rest s) score depth in-garbage true garbage-count)
                           (= ch \>) (recur (rest s) score depth false cancel-next garbage-count)
                           :else (recur (rest s) score depth in-garbage cancel-next (inc garbage-count)))
                         (case ch
                           \{ (recur (rest s) score (inc depth) in-garbage cancel-next garbage-count)
                           \} (recur (rest s) (+ score depth) (dec depth) in-garbage cancel-next garbage-count)
                           \< (recur (rest s) score depth true cancel-next garbage-count)
                           (recur (rest s) score depth in-garbage cancel-next garbage-count)))))))]
    (println result)))

(solve)
