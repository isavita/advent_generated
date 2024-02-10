(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn count-valid-digits [output]
  (count (filter #(contains? #{2 4 3 7} (count %)) (str/split output #" "))))

(defn main []
  (with-open [file (io/reader "input.txt")]
    (let [lines (line-seq file)
          count (reduce (fn [acc line]
                          (let [[_ output] (str/split line #" \| ")]
                            (+ acc (count-valid-digits output))))
                        0
                        lines)]
      (println count))))

(main)