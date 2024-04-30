(ns rock-paper-scissors
  (:require [clojure.java.io :as io]))

(defn calculate-score [line]
  (let [opponent (nth line 0)
        your-move (nth line 2)
        score (case your-move
                \X 1
                \Y 2
                \Z 3)]
    (cond
      (or (and (= opponent \A) (= your-move \Y))
          (and (= opponent \B) (= your-move \Z))
          (and (= opponent \C) (= your-move \X))) (+ score 6)
      (or (and (= opponent \A) (= your-move \X))
          (and (= opponent \B) (= your-move \Y))
          (and (= opponent \C) (= your-move \Z))) (+ score 3)
      :else score)))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [total-score (reduce + (map calculate-score (line-seq rdr)))]
      (println total-score))))

(-main)