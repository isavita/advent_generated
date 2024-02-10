(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn calculate-value-after-zero []
  (let [data (slurp "input.txt")
        steps (Integer/parseInt (str/trim data))
        steps (Integer/parseInt (str/trim data))
        current-pos (atom 0)
        value-after-zero (atom 0)]
    
    (doseq [i (range 1 50000001)]
      (swap! current-pos #(mod (+ % steps) i))
      (when (= @current-pos 0)
        (reset! value-after-zero i))
      (swap! current-pos inc))
    
    @value-after-zero))

(println (calculate-value-after-zero))