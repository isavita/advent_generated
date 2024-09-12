(defn find-marker [s marker-size]
  (loop [index 0
         window (vec (take marker-size s))]
    (if (= (count (set window)) marker-size)
      (+ index marker-size)
      (recur (inc index)
             (conj (subvec window 1) (nth s (+ index marker-size)))))))

(let [input (slurp "input.txt")]
  (println "Part One:" (find-marker input 4))
  (println "Part Two:" (find-marker input 14)))
