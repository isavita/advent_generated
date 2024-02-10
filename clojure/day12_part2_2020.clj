
(defn rotate [x y angle]
  (let [radians (* angle (/ Math/PI 180))
        new-x (Math/round (+ (* x (Math/cos radians)) (* y (Math/sin radians))))
        new-y (Math/round (- (* y (Math/cos radians)) (* x (Math/sin radians))))]
    [new-x new-y]))

(defn move-ship [instructions]
  (loop [x 0
         y 0
         wx 10
         wy 1
         instructions instructions]
    (if (empty? instructions)
      (+ (Math/abs x) (Math/abs y))
      (let [[action value] (first instructions)
            remaining (rest instructions)]
        (cond
          (= action \N) (recur x y wx (+ wy value) remaining)
          (= action \S) (recur x y wx (- wy value) remaining)
          (= action \E) (recur x y (+ wx value) wy remaining)
          (= action \W) (recur x y (- wx value) wy remaining)
          (= action \L) (recur x y (first (rotate wx wy (- value))) (second (rotate wx wy (- value))) remaining)
          (= action \R) (recur x y (first (rotate wx wy value)) (second (rotate wx wy value)) remaining)
          (= action \F) (recur (+ x (* value wx)) (+ y (* value wy)) wx wy remaining))))))

(with-open [rdr (clojure.java.io/reader "input.txt")]
  (let [input (line-seq rdr)
        result (move-ship (map (fn [s] [(first s) (Integer/parseInt (subs s 1))]) input))]
    (println result)))
