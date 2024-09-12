(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-path [path]
  (map #(vector (first %) (Integer/parseInt (subs % 1))) (str/split path #",")))

(defn generate-coords [path]
  (loop [steps path
         x 0
         y 0
         coords #{}]
    (if (empty? steps)
      coords
      (let [[dir dist] (first steps)
            new-coords (case dir
                         \R (set (map #(vector (+ x %) y) (range 1 (inc dist))))
                         \L (set (map #(vector (- x %) y) (range 1 (inc dist))))
                         \U (set (map #(vector x (+ y %)) (range 1 (inc dist))))
                         \D (set (map #(vector x (- y %)) (range 1 (inc dist)))))]
        (recur (rest steps)
               (case dir \R (+ x dist) \L (- x dist) x)
               (case dir \U (+ y dist) \D (- y dist) y)
               (set/union coords new-coords))))))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solve-wire-crossing []
  (let [[wire1 wire2] (str/split-lines (slurp "input.txt"))
        path1 (parse-path wire1)
        path2 (parse-path wire2)
        coords1 (generate-coords path1)
        coords2 (generate-coords path2)
        intersections (set/intersection coords1 coords2)]
    (apply min (map manhattan-distance intersections))))

(println (solve-wire-crossing))
