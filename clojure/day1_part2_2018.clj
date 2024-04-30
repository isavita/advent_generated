(defn solve []
  (let [lines (clojure.string/split-lines (slurp "input.txt"))
        frequencies (atom #{})
        current-frequency (atom 0)]
    (swap! frequencies conj @current-frequency)
    (loop []
      (doseq [line lines]
        (let [delta (Integer/parseInt line)]
          (swap! current-frequency + delta)
          (if (@frequencies @current-frequency)
            (do (println @current-frequency)
                (System/exit 0))
            (swap! frequencies conj @current-frequency))))
      (recur))))

(solve)