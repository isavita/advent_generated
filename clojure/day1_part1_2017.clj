
(ns solution
  (:require [clojure.java.io :as io]))

(defn- read-input []
  (-> (slurp (io/reader "input.txt"))
      (clojure.string/trim)))

(defn -main []
  (let [input (read-input)
        sum (atom 0)]
    (doseq [i (range (count input))]
      (let [next (mod (inc i) (count input))]
        (when (= (get input i) (get input next))
          (swap! sum + (Integer/parseInt (str (get input i)))))))
    (println @sum)))

(-main)
