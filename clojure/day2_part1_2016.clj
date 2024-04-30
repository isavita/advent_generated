(ns bathroom-security
  (:gen-class))

(def keypad [[1 2 3] [4 5 6] [7 8 9]])

(defn move [[x y] direction]
  (case direction
    "U" [(max 0 (dec x)) y]
    "D" [(min 2 (inc x)) y]
    "L" [x (max 0 (dec y))]
    "R" [x (min 2 (inc y))]))

(defn find-button [instructions]
  (let [start [1 1]]
    (reduce (fn [pos instruction]
              (move pos (str instruction)))
            start instructions)))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [instructions (line-seq rdr)]
      (println (reduce (fn [code instruction]
                         (let [[x y] (find-button (seq instruction))]
                           (str code (get-in keypad [x y])))
                       ) "" instructions)))))

(-main)