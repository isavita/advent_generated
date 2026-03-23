
(require '[clojure.string :as str])
(import '[clojure.lang PersistentQueue])

(defn solve []
  (let [input (try (slurp "input.txt") (catch Exception _ ""))
        lines (str/split-lines input)
        grid (mapv vec lines)
        h (count grid)
        w (if (pos? h) (count (first grid)) 0)
        coords (for [y (range h) x (range w)] [y x])
        start (first (for [[y x] coords :when (= (get-in grid [y x]) \@)] [y x]))
        all-k (reduce (fn [m [y x]]
                        (let [c (get-in grid [y x])
                              ci (int c)]
                          (if (<= 97 ci 122) (bit-or m (bit-shift-left 1 (- ci 97))) m)))
                      0 coords)]
    (if (zero? all-k)
      (println "Shortest path to collect all keys is: 0")
      (loop [q (conj PersistentQueue/EMPTY [start 0 0])
             v #{[(first start) (second start) 0]}]
        (when-let [[[y x] mask dist] (peek q)]
          (let [cands (for [[dy dx] [[0 1] [0 -1] [1 0] [-1 0]]
                            :let [ny (+ y dy) nx (+ x dx)]
                            :when (and (>= ny 0) (< ny h) (>= nx 0) (< nx w))
                            :let [c (get-in grid [ny nx])
                                  ci (int c)
                                  kb (if (<= 97 ci 122) (bit-shift-left 1 (- ci 97)) 0)
                                  db (if (<= 65 ci 90) (bit-shift-left 1 (- ci 65)) 0)
                                  nm (bit-or mask kb)]
                            :when (and (not= c \#)
                                       (or (zero? db) (not= 0 (bit-and mask db)))
                                       (not (v [ny nx nm])))]
                        [ny nx nm])]
            (if-let [win (some #(when (= (nth % 2) all-k) %) cands)]
              (println "Shortest path to collect all keys is:" (inc dist))
              (recur (reduce (fn [acc [ny nx nm]] (conj acc [[ny nx] nm (inc dist)])) (pop q) cands)
                     (into v cands)))))))))

(solve)
