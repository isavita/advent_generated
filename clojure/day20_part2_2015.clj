
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn -main [& _]
  (let [target (Long/parseLong (str/trim (slurp "input.txt")))
        max-houses 50
        multiplier 11
        houses (long-array (inc (quot target 10)))]
    (loop [elf 1]
      (when (< elf (quot target 10))
        (loop [house elf cnt 0]
          (when (and (< house (alength houses)) (< cnt max-houses))
            (aset houses house (+ (aget houses house) (* elf multiplier)))
            (recur (+ house elf) (inc cnt))))
        (recur (inc elf))))
    (println (* 10 (loop [i 0]
                     (if (>= (aget houses i) target) i (recur (inc i))))))))

(-main)
