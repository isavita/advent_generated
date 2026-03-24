
(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(defn solve []
  (let [grid (char-array 2100000 \space)
        max-y (atom 0)]
    (with-open [rdr (io/reader "input.txt")]
      (doseq [line (line-seq rdr)]
        (let [[_ axis v1 v2 v3] (re-find #"(x|y)=(\d+), [xy]=(\d+)\.\.(\d+)" line)
              a (Integer/parseInt v1)
              b (Integer/parseInt v2)
              c (Integer/parseInt v3)]
          (if (= axis "x")
            (do (doseq [y (range b (inc c))] (aset grid (+ (* y 1000) a) \#))
                (swap! max-y max c))
            (do (doseq [x (range b (inc c))] (aset grid (+ (* a 1000) x) \#))
                (swap! max-y max a))))))
    (letfn [(solid? [x y] (and (<= y @max-y) (#{\# \~} (aget grid (+ (* y 1000) x)))))
            (fill [x y]
              (let [idx (+ (* y 1000) x)]
                (when (and (<= y @max-y) (not (#{\# \~} (aget grid idx))))
                  (let [f (= (aget grid idx) \|)]
                    (aset grid idx \|)
                    (when-not f (fill x (inc y)))
                    (when (solid? x (inc y))
                      (let [l (loop [lx x]
                                (let [nx (dec lx) n-idx (+ (* y 1000) nx)]
                                  (cond (< nx 0) [lx false]
                                        (= (aget grid n-idx) \#) [lx true]
                                        :else (do (let [f2 (= (aget grid n-idx) \|)]
                                                    (aset grid n-idx \|)
                                                    (when-not f2 (fill nx (inc y))))
                                                  (if (solid? nx (inc y)) (recur nx) [nx false])))))
                            r (loop [rx x]
                                (let [nx (inc rx) n-idx (+ (* y 1000) nx)]
                                  (cond (>= nx 1000) [rx false]
                                        (= (aget grid n-idx) \#) [rx true]
                                        :else (do (let [f2 (= (aget grid n-idx) \|)]
                                                    (aset grid n-idx \|)
                                                    (when-not f2 (fill nx (inc y))))
                                                  (if (solid? nx (inc y)) (recur nx) [nx false])))))]
                        (when (and (second l) (second r))
                          (doseq [ix (range (first l) (inc (first r)))]
                            (aset grid (+ (* y 1000) ix) \~)))))))))]
      (fill 500 0)
      (let [m (* (inc @max-y) 1000)]
        (loop [i 0 c 0]
          (if (< i m)
            (recur (inc i) (if (= (aget grid i) \~) (inc c) c))
            (println c)))))))

(solve)
