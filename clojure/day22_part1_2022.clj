
(require '[clojure.string :as str])

(defn get-cell [g r c]
  (let [row (get g r)]
    (when (and row (>= c 0) (< c (count row)))
      (let [ch (.charAt ^String row c)]
        (when (not= ch \space) ch)))))

(defn step [g [r c] [dr dc]]
  (let [nr (+ r dr) nc (+ c dc)
        v (get-cell g nr nc)]
    (if v 
      (if (= v \#) [r c] [nr nc])
      (let [[wr wc] (loop [cr r cc c]
                      (let [pr (- cr dr) pc (- cc dc)]
                        (if (get-cell g pr pc) (recur pr pc) [cr cc])))]
        (if (= (get-cell g wr wc) \#) [r c] [wr wc])))))

(let [input (slurp "input.txt")
      [m-str p-str] (str/split input #"\r?\n\r?\n")
      grid (str/split-lines m-str)
      path (re-seq #"\d+|[RL]" (str/trim p-str))
      dirs [[0 1] [1 0] [0 -1] [-1 0]]]
  (loop [pos [0 (str/index-of (grid 0) ".")]
         di 0
         cmds path]
    (if-let [c (first cmds)]
      (case c
        "R" (recur pos (mod (inc di) 4) (rest cmds))
        "L" (recur pos (mod (+ di 3) 4) (rest cmds))
        (let [np (loop [p pos s (Integer/parseInt c)]
                   (if (zero? s) p
                     (let [nx (step grid p (dirs di))]
                       (if (= nx p) p (recur nx (dec s))))))]
          (recur np di (rest cmds))))
      (let [[r c] pos]
        (println (+ (* 1000 (inc r)) (* 4 (inc c)) di))))))
