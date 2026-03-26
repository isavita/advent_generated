
(require '[clojure.java.io :as io])

(defn largest-subseq [s k]
  (let [n (count s)
        to-remove (- n k)]
    (loop [chars (seq s)
           stack []
           rem to-remove]
      (if (seq chars)
        (let [c (first chars)]
          (if (and (pos? rem) (seq stack) (< (int (peek stack)) (int c)))
            (recur chars (pop stack) (dec rem))
            (recur (next chars) (conj stack c) rem)))
        (apply str (take k stack))))))

(defn solve []
  (try
    (with-open [rdr (io/reader "input.txt")]
      (let [total (reduce +
                          (for [line (line-seq rdr)
                                :let [s (.trim line)]
                                :when (seq s)]
                            (Long/parseLong (largest-subseq s 12))))]
        (println total)))
    (catch Exception e nil)))

(solve)
