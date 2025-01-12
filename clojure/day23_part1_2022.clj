
(defn parse-input [file-path]
  (with-open [rdr (clojure.java.io/reader file-path)]
    (let [lines (line-seq rdr)]
      (reduce (fn [acc [row line]]
                (reduce (fn [acc2 [col char]]
                          (if (= char \#)
                            (conj acc2 {:pos [row col] :moving false})
                            acc2))
                        acc
                        (map-indexed vector line)))
              []
              (map-indexed vector lines)))))

(defn around-all-empty? [elves pos map-elves]
  (not (some (fn [d] (contains? map-elves (mapv + pos d)))
             [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]])))

(defn elf-in-direction? [elves pos dir map-elves]
  (some (fn [j]
          (let [dxy (get [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]]
                         (mod (+ dir j) 8))]
            (contains? map-elves (mapv + pos dxy))))
        [-1 0 1]))

(defn run [elves curr-dir]
  (let [proposes (atom {})
        new-elves (mapv (fn [elf]
                          (if (around-all-empty? elves (:pos elf) (into #{} (map :pos elves)))
                            elf
                            (loop [i 0]
                              (if (= i 4)
                                elf
                                (let [dir (get [1 5 7 3] (mod (+ curr-dir i) 4))]
                                  (if (elf-in-direction? elves (:pos elf) dir (into #{} (map :pos elves)))
                                    (recur (inc i))
                                    (let [dxy (get [[-1 0] [1 0] [0 -1] [0 1]] (mod (+ curr-dir i) 4))
                                          next-pos (mapv + (:pos elf) dxy)]
                                      (swap! proposes update next-pos (fn [x] (if x (inc x) 1)))
                                      (assoc elf :next-pos next-pos :moving true))))))))
                        elves)]
    (let [moved-elves (mapv (fn [elf]
                             (if (not (:moving elf))
                               elf
                               (if (> (get @proposes (:next-pos elf) 0) 1)
                                 (assoc elf :moving false)
                                 (assoc elf :pos (:next-pos elf) :moving false))))
                           new-elves)]
      [(not= moved-elves elves) moved-elves (mod (inc curr-dir) 4)])))

(defn solve [elves]
  (loop [elves elves
         curr-dir 0
         i 0]
    (if (= i 10)
      elves
      (let [[moved? new-elves new-curr-dir] (run elves curr-dir)]
        (recur new-elves new-curr-dir (inc i))))))

(defn min-max [elves]
  (let [positions (map :pos elves)
        xs (map first positions)
        ys (map second positions)]
    (let [min-x (apply min xs)
          max-x (apply max xs)
          min-y (apply min ys)
          max-y (apply max ys)]
      [[min-x min-y] [max-x max-y]])))

(defn count-empty [elves]
  (let [[[min-x min-y] [max-x max-y]] (min-max elves)
        map-elves (into #{} (map :pos elves))]
    (reduce (fn [count x]
              (reduce (fn [count2 y]
                        (if (not (contains? map-elves [x y]))
                          (inc count2)
                          count2))
                      count
                      (range min-y (inc max-y))))
            0
            (range min-x (inc max-x)))))

(defn -main []
  (let [elves (parse-input "input.txt")
        final-elves (solve elves)]
    (println (count-empty final-elves))))

(-main)
