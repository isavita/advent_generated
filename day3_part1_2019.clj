
(ns solution
  (:require [clojure.string :as str]))

(defn get-points [path]
  (let [points (atom #{})
        current (atom [0 0])]
    (doseq [move (str/split path #",")]
      (let [dir (first move)
            steps (Integer/parseInt (str/join (rest move)))]
        (dotimes [_ steps]
          (case dir
            \U (swap! current #(vec [(first %) (inc (second %))]))
            \D (swap! current #(vec [(first %) (dec (second %))]))
            \L (swap! current #(vec [(dec (first %)) (second %)]))
            \R (swap! current #(vec [(inc (first %)) (second %)]))
          )
          (swap! points conj @current)
        )
      )
    )
    @points
  )
)

(defn abs [x]
  (if (< x 0)
    (- x)
    x
  )
)

(defn -main []
  (let [data (slurp "input.txt")
        lines (str/split (str/trim data) #"\n")
        wire1 (get-points (first lines))
        wire2 (get-points (second lines))
        intersections (reduce #(if (contains? wire2 %2) (conj %1 %2) %1) #{} wire1)
        min-distance (reduce min (map #(apply + (map abs %)) intersections))]
    (println min-distance)
  )
)

(-main)
