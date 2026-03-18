
(require '[clojure.string :as str])

(defn read-input []
  (slurp "input.txt"))

(defn abs [x]
  (if (neg? x) (- x) x))

(defn sign [n]
  (cond
    (pos? n) 1
    (neg? n) -1
    :else 0))

(def dir-map
  {\^ [0 1], \> [1 0], \v [0 -1], \< [-1 0]
   \U [0 1], \R [1 0], \D [0 -1], \L [-1 0]
   \N [0 1], \E [1 0], \S [0 -1], \W [-1 0]})

(defn dir-from-byte [b]
  (dir-map b))

(defn update-head [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn next-point [[hx hy] [tx ty]]
  (if (and (<= (abs (- hx tx)) 1) (<= (abs (- hy ty)) 1))
    [tx ty]
    [(+ tx (sign (- hx tx))) (+ ty (sign (- hy ty)))]))

(defn update-rope [rope dir]
  (let [d (dir-from-byte dir)
        new-head (update-head (first rope) d)]
    (loop [i 1
           new-rope [new-head]]
      (if (< i (count rope))
        (let [prev (new-rope (dec i))
              curr (rope i)
              new-curr (next-point prev curr)]
          (recur (inc i) (conj new-rope new-curr)))
        new-rope))))

(defn simulate [input rope-length]
  (let [rope (vec (repeat rope-length [0 0]))
        lines (remove str/blank? (str/split-lines input))]
    (loop [lines lines
           rope rope
           visited #{[0 0]}]
      (if (empty? lines)
        (count visited)
        (let [line (first lines)
              parts (str/split line #"\s+")]
          (if (not= 2 (count parts))
            (recur (rest lines) rope visited)
            (let [[dir-str count-str] parts
                  dir (first dir-str)
                  count (Integer/parseInt count-str)
                  [new-rope new-visited]
                  (reduce (fn [[r v] _]
                            (let [new-r (update-rope r dir)]
                              [new-r (conj v (last new-r))]))
                          [rope visited]
                          (range count))]
              (recur (rest lines) new-rope new-visited))))))))

(defn -main []
  (let [input (read-input)]
    (println (simulate input 10))))

(-main)
