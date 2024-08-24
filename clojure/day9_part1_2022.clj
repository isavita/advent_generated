(ns rope-bridge
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn move [pos dir]
  (case dir
    "R" [(inc (first pos)) (second pos)]
    "L" [(dec (first pos)) (second pos)]
    "U" [(first pos) (inc (second pos))]
    "D" [(first pos) (dec (second pos))]))

(defn distance [p1 p2]
  (max (Math/abs (- (first p1) (first p2)))
       (Math/abs (- (second p1) (second p2)))))

(defn follow [head tail]
  (let [dx (- (first head) (first tail))
        dy (- (second head) (second tail))]
    (cond
      (and (<= (Math/abs dx) 1) (<= (Math/abs dy) 1)) tail
      (zero? dx) [(first tail) (+ (second tail) (quot dy (Math/abs dy)))]
      (zero? dy) [(+ (first tail) (quot dx (Math/abs dx))) (second tail)]
      :else [(+ (first tail) (quot dx (Math/abs dx)))
             (+ (second tail) (quot dy (Math/abs dy)))])))

(defn solve [input]
  (let [lines (str/split-lines input)
        moves (map #(str/split % #" ") lines)
        head (atom [0 0])
        tail (atom [0 0])
        visited (atom #{[0 0]})]
    (doseq [[dir steps] moves]
      (dotimes [_ (Integer/parseInt steps)]
        (let [new-head (move @head dir)]
          (reset! head new-head)
          (reset! tail (follow new-head @tail))
          (swap! visited conj @tail))))
    (count @visited)))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [input (slurp r)]
      (println (solve input)))))

(-main)