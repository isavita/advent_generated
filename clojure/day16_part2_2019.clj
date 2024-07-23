
(defn read-input [filename]
  (-> (slurp filename)
      (clojure.string/trim)))

(defn repeat-input [input times]
  (let [digits (map #(Integer. (str %)) input)]
    (vec (apply concat (repeat times digits)))))

(defn fft-phase [input offset]
  (let [len (count input)]
    (loop [i (dec len) sum 0 result (vec input)]
      (if (>= i offset)
        (let [new-sum (+ sum (nth result i))]
          (recur (dec i) (mod new-sum 10) (assoc result i (mod new-sum 10))))
        result))))

(defn fft [input offset phases]
  (reduce (fn [acc _] (fft-phase acc offset)) input (range phases)))

(defn main []
  (let [input (read-input "input.txt")
        repeated-input (repeat-input input 10000)
        offset (Integer. (subs input 0 7))
        final-output (fft repeated-input offset 100)]
    (println (apply str (subvec final-output offset (+ offset 8))))))

(main)
