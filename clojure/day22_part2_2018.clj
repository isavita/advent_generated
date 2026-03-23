
(ns solution
  (:import [java.util PriorityQueue Comparator]))

(defn solve []
  (let [input (slurp "input.txt")
        depth (Long/parseLong (second (re-find #"depth:\s*(\d+)" input)))
        coords (re-find #"target:\s*(\d+),(\d+)" input)
        tx (Integer/parseInt (nth coords 1))
        ty (Integer/parseInt (nth coords 2))
        max-x (+ tx 100)
        max-y (+ ty 100)
        erosion (make-array Long/TYPE max-y max-x)]
    (doseq [y (range max-y)
            x (range max-x)]
      (let [gi (cond
                 (and (zero? x) (zero? y)) 0
                 (and (= x tx) (= y ty)) 0
                 (zero? y) (* (long x) 16807)
                 (zero? x) (* (long y) 48271)
                 :else (* (aget erosion y (dec x))
                          (aget erosion (dec y) x)))]
        (aset erosion y x (mod (+ gi depth) 20183))))
    (let [get-type (fn [x y] (int (mod (aget erosion y x) 3)))
          v-idx (fn [x y t] (+ (* x max-y 3) (* y 3) t))
          visited (boolean-array (* max-x max-y 3))
          pq (PriorityQueue. 100000 (reify Comparator 
                                     (compare [_ a b] 
                                       (Long/compare (aget ^longs a 0) (aget ^longs b 0)))))]
      (.add pq (long-array [0 0 0 1]))
      (loop []
        (when-let [^longs curr (.poll pq)]
          (let [time (aget curr 0)
                x (int (aget curr 1))
                y (int (aget curr 2))
                tool (int (aget curr 3))
                idx (v-idx x y tool)]
            (if (aget visited idx)
              (recur)
              (do
                (aset visited idx true)
                (if (and (= x tx) (= y ty) (= tool 1))
                  (println time)
                  (do
                    (let [nt (- 3 tool (get-type x y))]
                      (.add pq (long-array [(+ time 7) x y nt])))
                    (doseq [[dx dy] [[1 0] [-1 0] [0 1] [0 -1]]]
                      (let [nx (+ x dx) ny (+ y dy)]
                        (when (and (>= nx 0) (< nx max-x) (>= ny 0) (< ny max-y))
                          (when (not= tool (get-type nx ny))
                            (.add pq (long-array [(+ time 1) nx ny tool]))))))
                    (recur)))))))))))

(solve)
