
(import '[java.util LinkedList])

(defn solve []
  (let [content (try (slurp "input.txt") (catch Exception _ ""))
        pts (->> (re-seq #"-?\d+" content)
                 (map #(Long/parseLong %))
                 (partition 2)
                 (map vec)
                 vec)
        n (count pts)]
    (if (zero? n)
      (do (println "Largest valid area: 0") (System/exit 0)))
    (let [xs (vec (sort (distinct (map first pts))))
          ys (vec (sort (distinct (map second pts))))
          ux (count xs) uy (count ys)
          xidx (zipmap xs (range)) yidx (zipmap ys (range))
          W (inc (* 2 ux)) H (inc (* 2 uy))
          col-w (long-array W) row-h (long-array H)]
      (aset col-w 0 1)
      (dotimes [i ux]
        (aset col-w (inc (* 2 i)) 1)
        (aset col-w (+ (* 2 i) 2) (if (< (inc i) ux) (max 0 (dec (- (xs (inc i)) (xs i)))) 1)))
      (aset row-h 0 1)
      (dotimes [i uy]
        (aset row-h (inc (* 2 i)) 1)
        (aset row-h (+ (* 2 i) 2) (if (< (inc i) uy) (max 0 (dec (- (ys (inc i)) (ys i)))) 1)))
      (let [grid (make-array Integer/TYPE H W)]
        (dotimes [i n]
          (let [[x1 y1] (pts i) [x2 y2] (pts (mod (inc i) n))
                gx1 (inc (* 2 (xidx x1))) gy1 (inc (* 2 (yidx y1)))
                gx2 (inc (* 2 (xidx x2))) gy2 (inc (* 2 (yidx y2)))]
            (if (= gx1 gx2)
              (doseq [y (range (min gy1 gy2) (inc (max gy1 gy2)))] (aset-int (aget grid y) gx1 1))
              (doseq [x (range (min gx1 gx2) (inc (max gx1 gx2)))] (aset-int (aget grid gy1) x 1)))))
        (let [q (LinkedList. [[0 0]])]
          (aset-int (aget grid 0) 0 2)
          (while (not (.isEmpty q))
            (let [[cx cy] (.poll q)]
              (doseq [[dx dy] [[0 1] [0 -1] [1 0] [-1 0]]]
                (let [nx (+ cx dx) ny (+ cy dy)]
                  (when (and (>= nx 0) (< nx W) (>= ny 0) (< ny H)
                             (zero? (aget (aget grid ny) nx)))
                    (aset-int (aget grid ny) nx 2)
                    (.add q [nx ny])))))))
        (let [P (make-array Long/TYPE H W)]
          (dotimes [y H]
            (let [rh (aget row-h y) r (aget grid y)]
              (loop [x 0 rs 0]
                (when (< x W)
                  (let [val (if (not= (aget r x) 2) (* (aget col-w x) rh) 0)
                        cur-rs (+ rs val)
                        above (if (pos? y) (aget (aget P (dec y)) x) 0)]
                    (aset-long (aget P y) x (+ cur-rs above))
                    (recur (inc x) cur-rs))))))
          (let [max-area (atom 0)]
            (dotimes [i n]
              (dotimes [j n]
                (let [[x1 y1] (pts i) [x2 y2] (pts j)
                      w (inc (Math/abs (- x1 x2))) h (inc (Math/abs (- y1 y2)))
                      area (* (long w) (long h))]
                  (when (> area @max-area)
                    (let [gx1 (inc (* 2 (xidx x1))) gy1 (inc (* 2 (yidx y1)))
                          gx2 (inc (* 2 (xidx x2))) gy2 (inc (* 2 (yidx y2)))
                          xmin (min gx1 gx2) xmax (max gx1 gx2)
                          ymin (min gy1 gy2) ymax (max gy1 gy2)
                          total (aget (aget P ymax) xmax)
                          left (if (pos? xmin) (aget (aget P ymax) (dec xmin)) 0)
                          up (if (pos? ymin) (aget (aget P (dec ymin)) xmax) 0)
                          diag (if (and (pos? xmin) (pos? ymin)) (aget (aget P (dec ymin)) (dec xmin)) 0)]
                      (if (= (- (+ total diag) left up) area) (reset! max-area area)))))))
            (println "Largest valid area:" @max-area)))))))

(solve)
