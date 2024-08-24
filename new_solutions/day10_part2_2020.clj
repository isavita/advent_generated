(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input []
  (with-open [rdr (io/reader "input.txt")]
    (doall (map #(Integer/parseInt %) (line-seq rdr)))))

(defn solve []
  (let [adapters (read-input)
        sorted-adapters (sort adapters)
        device-adapter (+ 3 (apply max sorted-adapters))
        all-adapters (conj sorted-adapters device-adapter 0)
        sorted-all-adapters (sort all-adapters)
        dp (atom {0 1})]
    (doseq [adapter (rest sorted-all-adapters)]
      (swap! dp assoc adapter
             (+ (get @dp (- adapter 1) 0)
                (get @dp (- adapter 2) 0)
                (get @dp (- adapter 3) 0))))
    (println (@dp device-adapter))))

(solve)