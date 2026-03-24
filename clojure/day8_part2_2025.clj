
(require '[clojure.string :as str])

(defn solve []
  (let [content (try (slurp "input.txt") (catch Exception _ ""))
        pts (->> (str/split-lines content)
                 (keep #(let [v (str/split % #",")]
                          (when (= 3 (count v)) (mapv parse-long v))))
                 vec)
        n (count pts)]
    (when (> n 1)
      (let [find-root (fn find-root [m i] (if (= (m i) i) i (recur m (m i))))
            edges (sort-by last (for [i (range n) j (range (inc i) n)]
                                  [i j (let [[x1 y1 z1] (pts i)
                                             [x2 y2 z2] (pts j)
                                             dx (- x1 x2) dy (- y1 y2) dz (- z1 z2)]
                                         (+ (* dx dx) (* dy dy) (* dz dz)))]))]
        (loop [[[u v] & r] edges p (vec (range n)) c n]
          (when (and u v)
            (let [ru (find-root p u)
                  rv (find-root p v)]
              (if (not= ru rv)
                (if (= c 2)
                  (let [[x1 y1 z1] (pts u)
                        [x2 y2 z2] (pts v)]
                    (printf "Connected %d,%d,%d and %d,%d,%d\n" x1 y1 z1 x2 y2 z2)
                    (println "Product of X coordinates:" (* x1 x2)))
                  (recur r (assoc p ru rv) (dec c)))
                (recur r p c)))))))))

(solve)
