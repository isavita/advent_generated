
(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (map vec (line-seq rdr)))))

(def directions
  [[-1 -1] [0 -1] [1 -1]
   [-1 0]  [1 0]
   [-1 1] [0 1] [1 1]])

(defn count-visible-occupied [seating-area row col]
  (let [rows (count seating-area)
        cols (count (first seating-area))]
    (reduce + 0
            (for [dir directions]
              (loop [r (+ row (second dir))
                     c (+ col (first dir))]
                (cond
                  (or (< r 0) (>= r rows) (< c 0) (>= c cols)) 0
                  (= (get-in seating-area [r c]) \L) 0
                  (= (get-in seating-area [r c]) \#) 1
                  :else (recur (+ r (second dir)) (+ c (first dir)))))))))

(defn simulate-seating [seating-area]
  (let [rows (count seating-area)
        cols (count (first seating-area))
        new-seating-area (vec (for [i (range rows)] (vec (for [j (range cols)] (get-in seating-area [i j])))))]
    (loop [changed false
           i 0
           j 0
           new-seating-area new-seating-area]
      (cond
        (>= i rows) [new-seating-area changed]
        (>= j cols) (recur changed (inc i) 0 new-seating-area)
        :else
        (let [seat (get-in seating-area [i j])]
          (case seat
            \L (if (zero? (count-visible-occupied seating-area i j))
                 (recur true i (inc j) (assoc-in new-seating-area [i j] \#))
                 (recur changed i (inc j) new-seating-area))
            \# (if (>= (count-visible-occupied seating-area i j) 5)
                 (recur true i (inc j) (assoc-in new-seating-area [i j] \L))
                 (recur changed i (inc j) new-seating-area))
            (recur changed i (inc j) new-seating-area)))))))

(defn count-occupied-seats [seating-area]
  (reduce + 0 (for [row seating-area
                    seat row
                    :when (= seat \#)] 1)))

(defn solve []
  (loop [seating-area (read-input "input.txt")]
    (let [[new-seating-area changed] (simulate-seating seating-area)]
      (if (not changed)
        (count-occupied-seats new-seating-area)
        (recur new-seating-area)))))

(println (solve))
