(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input [filename]
  (with-open [reader (io/reader filename)]
    (doall (line-seq reader))))

(defn process-instructions [instructions]
  (let [cycles (atom [])
        x (atom 1)]
    (doseq [instruction instructions]
      (if (str/starts-with? instruction "noop")
        (swap! cycles conj @x)
        (let [value (Integer/parseInt (subs instruction 5))]
          (swap! cycles conj @x)
          (swap! cycles conj @x)
          (swap! x + value))))
    (swap! cycles conj @x)
    @cycles))

(defn calculate-signal-strengths [cycles]
  (apply + (for [i (range 20 (count cycles) 40)]
             (* i (nth cycles (dec i))))))

(defn -main []
  (let [instructions (read-input "input.txt")
        cycles (process-instructions instructions)
        signal-strengths (calculate-signal-strengths cycles)]
    (println signal-strengths)))

(-main)