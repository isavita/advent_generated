
(defn build-map [regex]
  (loop [regex regex
         stack []
         cp [0 0]
         dm {}]
    (if (empty? regex)
      dm
      (let [c (first regex)]
        (cond
          (= c \() (recur (rest regex) (conj stack cp) cp dm)
          (= c \|) (recur (rest regex) stack (peek stack) dm)
          (= c \)) (recur (rest regex) (pop stack) (peek stack) dm)
          :else (let [np (case c
                           \N [(first cp) (dec (second cp))]
                           \S [(first cp) (inc (second cp))]
                           \E [(inc (first cp)) (second cp)]
                           \W [(dec (first cp)) (second cp)])
                       new-dm (assoc dm cp (assoc (get dm cp {}) np true))]
                   (recur (rest regex) stack np new-dm)))))))

(defn find-furthest-room [dm]
  (loop [queue [[0 0]]
         visited {[0 0] 0}
         max-doors 0]
    (if (empty? queue)
      max-doors
      (let [p (first queue)
            neighbors (keys (get dm p {}))]
        (if (empty? neighbors)
          (recur (rest queue) visited max-doors)
          (let [unvisited-neighbors (remove #(contains? visited %) neighbors)
                new-visited (reduce #(assoc %1 %2 (inc (get visited p))) visited unvisited-neighbors)
                new-max-doors (max max-doors (apply max (vals new-visited)))]
            (recur (concat (rest queue) unvisited-neighbors) new-visited new-max-doors)))))))

(defn solve []
  (let [regex (slurp "input.txt")
        dm (build-map (subs regex 1 (dec (count regex))))]
    (find-furthest-room dm)))

(println (solve))
