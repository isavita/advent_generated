
(require '[clojure.string :as str])

(defn solve []
  (let [input (str/trim (slurp "input.txt"))
        total-size (reduce + (map #(- (int %) 48) input))
        disk (int-array total-size -1)
        files (atom [])
        pos (atom 0)
        file-id (atom 0)]
    
    (doseq [[i len] (map-indexed vector (map #(- (int %) 48) input))]
      (if (even? i)
        (do
          (swap! files conj [@file-id @pos (dec (+ @pos len))])
          (dotimes [j len]
            (aset disk (+ @pos j) @file-id))
          (swap! file-id inc))
        (dotimes [j len]
          (aset disk (+ @pos j) -1)))
      (swap! pos + len))
    
    (doseq [[id start end] (reverse @files)]
      (let [file-len (inc (- end start))]
        (loop [j 0]
          (when (< j start)
            (if (= (aget disk j) -1)
              (let [span-len (loop [k j cnt 0]
                              (if (and (< k start) (= (aget disk k) -1))
                                (recur (inc k) (inc cnt))
                                cnt))]
                (if (>= span-len file-len)
                  (do
                    (dotimes [m file-len]
                      (aset disk (+ j m) id)
                      (aset disk (+ start m) -1))
                    (recur start))
                  (recur (+ j (max 1 span-len)))))
              (recur (inc j)))))))
    
    (areduce disk i acc (long 0)
      (let [val (aget disk i)]
        (if (not= val -1)
          (+ acc (* i val))
          acc)))))

(println (solve))
