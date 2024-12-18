
(defn read-all [path]
  (-> path slurp clojure.string/trim))

(defn to-int [s]
  (Integer/parseInt s))

(defn mix [nums]
  (let [n (dec (count nums))]
    (loop [nums nums i 0]
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

(let [nums (->> (read-all "input.txt")
                (clojure.string/split-lines)
                (map-indexed (fn [i n] {:pos i :val (to-int n)})))
      nums2 (mapv #(assoc % :val (* 811589153 (:val %))) nums)]
  (->> (range 10)
       (reduce (fn [nums _] (mix nums)) nums2)
       coords
       prn))
