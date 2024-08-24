(ns day14
  (:gen-class))

(defn make-recipes [input]
  (let [scores (atom [3 7])
        elf1 (atom 0)
        elf2 (atom 1)]
    (loop []
      (let [sum (+ (nth @scores @elf1) (nth @scores @elf2))
            new-scores (if (>= sum 10) [(quot sum 10) (mod sum 10)] [(mod sum 10)])]
        (swap! scores into new-scores)
        (reset! elf1 (mod (+ @elf1 1 (nth @scores @elf1)) (count @scores)))
        (reset! elf2 (mod (+ @elf2 1 (nth @scores @elf2)) (count @scores)))
        (if (>= (count @scores) (+ input 10))
          (apply str (subvec @scores input (+ input 10)))
          (recur))))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [input (Integer/parseInt (first (line-seq rdr)))]
      (println (make-recipes input)))))

(-main)