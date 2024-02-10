
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn calculate-score [opponent round-end]
  (let [your-move (cond
                    (= round-end \X) (cond
                                       (= opponent \A) \Z
                                       (= opponent \B) \X
                                       :else \Y)
                    (= round-end \Y) (cond
                                       (= opponent \A) \X
                                       (= opponent \B) \Y
                                       :else \Z)
                    :else (cond
                            (= opponent \A) \Y
                            (= opponent \B) \Z
                            :else \X))]
    (let [score (cond
                  (= your-move \X) 1
                  (= your-move \Y) 2
                  :else 3)]
      (cond
        (or (and (= opponent \A) (= your-move \Y))
            (and (= opponent \B) (= your-move \Z))
            (and (= opponent \C) (= your-move \X))) (+ score 6)
        (or (and (= opponent \A) (= your-move \X))
            (and (= opponent \B) (= your-move \Y))
            (and (= opponent \C) (= your-move \Z))) (+ score 3)
        :else score))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [total-score (reduce +
                         (map (fn [line]
                                (let [[opponent round-end] (str/split line #"\s+")]
                                  (calculate-score (first opponent) (last round-end))))
                              (line-seq rdr)))]
      (println total-score))))

(-main)
