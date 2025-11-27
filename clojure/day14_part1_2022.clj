
(ns sand
  (:require [clojure.string :as str]))

(defn parse-input [txt]
  (->> (str/split-lines txt)
       (map #(str/split % #" -> "))
       (map (fn [line]
              (map (fn [p] (mapv parse-long (str/split p #","))) line)))))

(defn draw-cave [paths]
  (reduce (fn [cave path]
            (reduce (fn [c seg]
                      (let [[[x1 y1] [x2 y2]] seg
                            dx (cond (= x1 x2) 0 (< x1 x2) 1 :else -1)
                            dy (cond (= y1 y2) 0 (< y1 y2) 1 :else -1)]
                        (loop [x x1, y y1, c c]
                          (let [c (assoc c [x y] \#)]
                            (if (and (= x x2) (= y y2)) c
                                (recur (+ x dx) (+ y dy) c))))))
                    cave
                    (partition 2 1 path)))
          {} paths))

(defn simulate [cave]
  (let [floor (apply max (map second (keys cave)))]
    (loop [cave cave, cnt 0]
      (let [final (loop [x 500, y 0]
                    (cond
                      (> y floor) nil
                      (nil? (get cave [x (inc y)])) (recur x (inc y))
                      (nil? (get cave [(dec x) (inc y)])) (recur (dec x) (inc y))
                      (nil? (get cave [(inc x) (inc y)])) (recur (inc x) (inc y))
                      :else [x y]))]
        (if final
          (recur (assoc cave final \o) (inc cnt))
          cnt)))))

(defn -main []
  (let [paths (parse-input (slurp "input.txt"))]
    (println (simulate (draw-cave paths)))))

(-main)
