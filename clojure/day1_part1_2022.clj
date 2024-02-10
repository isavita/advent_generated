(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn calculate-max-calories []
  (with-open [reader (io/reader "input.txt")]
    (let [lines (line-seq reader)
          [max-calories _] (reduce
                            (fn [[max-calories current-calories] line]
                              (if (str/blank? line)
                                (if (> current-calories max-calories)
                                  [current-calories 0]
                                  [max-calories 0])
                                (let [calories (Integer/parseInt line)]
                                  [(max max-calories (+ current-calories calories)) (+ current-calories calories)])))
                            [0 0] lines)]
      (println max-calories))))

(calculate-max-calories)