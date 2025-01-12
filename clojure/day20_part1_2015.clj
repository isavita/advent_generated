
(defn solve []
  (let [input (slurp "input.txt")
        target (-> input Integer/parseInt (/ 10))
        houses (int-array (inc target))]
    (doseq [elf (range 1 (inc target))
            house (range elf (inc target) elf)]
      (aset houses house (+ (aget houses house) elf)))
    (println (first (keep-indexed #(when (>= %2 target) %1) houses)))))

(solve)
