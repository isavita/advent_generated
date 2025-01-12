
(defn read-lines [path]
  (-> path slurp clojure.string/split-lines))

(defn parse-int [s]
  (Integer/parseInt s))

(defn mix [nums]
  (let [n (dec (count nums))]
    (loop [nums nums
           i 0]
      (if (= i (count nums))
        nums
        (let [old-pos (:pos (nth nums i))
              new-pos (mod (+ old-pos (:val (nth nums i))) n)
              new-pos (if (neg? new-pos) (+ new-pos n) new-pos)
              nums (if (< old-pos new-pos)
                     (mapv (fn [num]
                             (if (and (> (:pos num) old-pos) (<= (:pos num) new-pos))
                               (update num :pos dec)
                               num))
                           nums)
                     (if (< new-pos old-pos)
                       (mapv (fn [num]
                               (if (and (>= (:pos num) new-pos) (< (:pos num) old-pos))
                                 (update num :pos inc)
                                 num))
                             nums)
                       nums))]
          (recur (assoc-in nums [i :pos] new-pos) (inc i)))))))

(defn coords [nums]
  (let [l (count nums)
        zero-pos (:pos (first (filter (fn [num] (= (:val num) 0)) nums)))
        positions (map #(mod (+ zero-pos %) l) [1000 2000 3000])]
    (reduce + (map :val (filter (fn [num] (some #(= (:pos num) %) positions)) nums)))))

(defn solve [path multiplier mix-times]
  (let [lines (read-lines path)
        nums (map-indexed (fn [i line] {:pos i :val (parse-int line)}) lines)
        nums (mapv (fn [num] (update num :val * multiplier)) nums)]
    (loop [nums nums
           i 0]
      (if (= i mix-times)
        (coords nums)
        (recur (mix nums) (inc i))))))

(println (solve "input.txt" 1 1))
(println (solve "input.txt" 811589153 10))
