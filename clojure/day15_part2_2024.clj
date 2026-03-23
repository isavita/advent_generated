
(require '[clojure.string :as str])

(defn collect-moves [g pos d]
  (loop [q (list pos) m #{}]
    (if (empty? q) m
      (let [c (first q) r (rest q)]
        (if (m c) (recur r m)
          (case (get g c \.)
            \# nil
            \. (recur r m)
            \O (recur (cons (mapv + c d) r) (conj m c))
            \@ (recur (cons (mapv + c d) r) (conj m c))
            \[ (let [p2 (mapv + c [1 0])]
                 (recur (cons (mapv + c d) (cons (mapv + p2 d) r)) (conj m c p2)))
            \] (recur (cons (mapv + c [-1 0]) r) m)
            (recur r m)))))))

(defn solve [input]
  (let [[g-str m-str] (str/split input #"\n\n")
        lines (str/split-lines g-str)
        grid (into {} (for [y (range (count lines))
                            x (range (count (get lines y)))
                            :let [c (get-in lines [y x])]
                            :when (not (#{\. \space \newline \return} c))]
                        [[x y] c]))
        moves (keep {\^ [0 -1] \v [0 1] \< [-1 0] \> [1 0]} m-str)
        start (some (fn [[k v]] (when (= v \@) k)) grid)]
    (loop [p start g grid ms moves]
      (if-let [d (first ms)]
        (if-let [m (collect-moves g p d)]
          (let [ng (reduce (fn [acc k] (assoc acc (mapv + k d) (g k))) (apply dissoc g m) m)]
            (recur (mapv + p d) ng (rest ms)))
          (recur p g (rest ms)))
        (reduce-kv (fn [s [x y] v] (if (#{\O \[} v) (+ s (* 100 y) x) s)) 0 g)))))

(defn scale-up [s]
  (str/join (map #(get {\# "##" \O "[]" \. ".." \@ "@."} % %) s)))

(let [input (str/trim (slurp "input.txt"))]
  (println (solve input))
  (println (solve (scale-up input))))

