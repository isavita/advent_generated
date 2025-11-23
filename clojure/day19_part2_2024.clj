
(ns towel
  (:require [clojure.string :as str]))

(defn count-ways [design patterns]
  (let [n (count design)
        dp (long-array (inc n) 0)]
    (aset dp 0 1)
    (doseq [i (range 1 (inc n))]
      (doseq [p patterns]
        (let [lp (count p)]
          (when (and (>= i lp)
                     (= (subs design (- i lp) i) p))
            (aset dp i (+ (aget dp i) (aget dp (- i lp))))))))
    (aget dp n)))

(defn -main [& _]
  (let [lines (str/split-lines (slurp "input.txt"))
        patterns (mapv str/trim (str/split (first lines) #","))
        designs (filter seq (map str/trim (drop 2 lines)))]
    (println (reduce + (map #(count-ways % patterns) designs)))))

(-main)
