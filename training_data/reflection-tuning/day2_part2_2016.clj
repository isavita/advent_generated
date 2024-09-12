(def input (clojure.string/split-lines (slurp "input.txt")))

(def keypad-1 
  {[0 0] \1 [1 0] \2 [2 0] \3
   [0 1] \4 [1 1] \5 [2 1] \6
   [0 2] \7 [1 2] \8 [2 2] \9})

(def keypad-2
  {[2 0] \1
   [1 1] \2 [2 1] \3 [3 1] \4
   [0 2] \5 [1 2] \6 [2 2] \7 [3 2] \8 [4 2] \9
   [1 3] \A [2 3] \B [3 3] \C
   [2 4] \D})

(defn move [pos direction keypad]
  (let [[x y] pos
        new-pos (case direction
                  \U [x (dec y)]
                  \D [x (inc y)]
                  \L [(dec x) y]
                  \R [(inc x) y])]
    (if (contains? keypad new-pos)
      new-pos
      pos)))

(defn process-instruction [start instruction keypad]
  (reduce #(move %1 %2 keypad) start instruction))

(defn solve [keypad start-pos]
  (reductions
   (fn [pos instruction]
     (process-instruction pos instruction keypad))
   start-pos
   input))

(defn part1 []
  (->> (solve keypad-1 [1 1])
       (map keypad-1)
       (apply str)))

(defn part2 []
  (->> (solve keypad-2 [0 2])
       (map keypad-2)
       (apply str)))

(println "Part 1:" (part1))
(println "Part 2:" (part2))
