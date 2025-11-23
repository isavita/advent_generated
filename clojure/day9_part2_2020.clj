
(require '[clojure.java.io :as io])

(defn -main [& _]
  (let [numbers (vec (map #(Long/parseLong %) (line-seq (io/reader "input.txt"))))
        target 14360655]
    (println
      (some (fn [i]
              (loop [j (inc i) sum (nth numbers i) mn (nth numbers i) mx (nth numbers i)]
                (cond
                  (>= j (count numbers)) nil
                  (> sum target) nil
                  (= sum target) (+ mn mx)
                  :else (let [v (nth numbers j)]
                          (recur (inc j) (+ sum v) (min mn v) (max mx v))))))
            (range (count numbers))))))
(-main)
