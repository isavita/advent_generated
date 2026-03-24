
(require '[clojure.string :as str])

(defn solve []
  (let [lines (str/split-lines (slurp "input.txt"))
        h (count lines)
        w (apply max (map count lines))
        grid (into {} (for [y (range h)
                            x (range (count (get lines y)))
                            :let [c (get-in lines [y x])]
                            :when (and c (not= c \space))]
                        [[x y] c]))
        portals (->> (for [[[x y] c] grid :when (Character/isLetter c)]
                       (let [c2r (grid [(inc x) y])
                             c2d (grid [x (inc y)])]
                         (concat
                          (when (and c2r (Character/isLetter c2r))
                            (let [n (str c c2r)
                                  p1 [(dec x) y]
                                  p2 [(+ x 2) y]
                                  e (if (= (grid p1) \.) p1 (when (= (grid p2) \.) p2))]
                              (when e [[n e]])))
                          (when (and c2d (Character/isLetter c2d))
                            (let [n (str c c2d)
                                  p1 [x (dec y)]
                                  p2 [x (+ y 2)]
                                  e (if (= (grid p1) \.) p1 (when (= (grid p2) \.) p2))]
                              (when e [[n e]]))))))
                     (apply concat)
                     (distinct)
                     (group-by first))
        start (second (first (get portals "AA")))
        end (second (first (get portals "ZZ")))
        tele (into {} (mapcat (fn [[_ ps]]
                                (if (= 2 (count ps))
                                  (let [p1 (second (first ps))
                                        p2 (second (second ps))]
                                    [[p1 p2] [p2 p1]])
                                  []))
                              portals))
        outer? (fn [[x y]] (or (= x 2) (= y 2) (= x (- w 3)) (= y (- h 3))))]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start 0 0])
           v #{[start 0]}]
      (when-let [[curr depth dist] (peek q)]
        (if (and (= curr end) (= depth 0))
          (println dist)
          (let [moves (for [d [[0 1] [0 -1] [1 0] [-1 0]]
                            :let [np (mapv + curr d)]
                            :when (= (grid np) \.)
                            :when (not (v [np depth]))]
                        [np depth])
                jump (when-let [tp (tele curr)]
                       (let [nd (if (outer? curr) (dec depth) (inc depth))]
                         (when (and (>= nd 0) (not (v [tp nd])))
                           [[tp nd]])))
                nexts (concat moves jump)]
            (recur (reduce (fn [acc [p d]] (conj acc [p d (inc dist)])) (pop q) nexts)
                   (into v nexts))))))))

(solve)
