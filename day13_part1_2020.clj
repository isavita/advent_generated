
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn solve []
  (with-open [file (io/reader "input.txt")]
    (let [lines (line-seq file)
          earliestDeparture (Integer/parseInt (first lines))
          busIDs (str/split (second lines) #",")]
      (loop [earliestBusID 0
             minWaitTime earliestDeparture
             [id & rest] busIDs]
        (if (nil? id)
          (println (* earliestBusID minWaitTime))
          (if (= id "x")
            (recur earliestBusID minWaitTime rest)
            (let [busID (Integer/parseInt id)
                  waitTime (- busID (mod earliestDeparture busID))]
              (if (< waitTime minWaitTime)
                (recur busID waitTime rest)
                (recur earliestBusID minWaitTime rest)))))))))
  
(solve)
