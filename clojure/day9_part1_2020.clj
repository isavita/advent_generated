
(ns day9
  (:require [clojure.string :as str]))

(defn parse-long [s]
  (Long/parseLong s))

(defn valid-next-number? [preamble num]
  (let [preamble-set (set preamble)]
    (some (fn [x]
            (let [diff (- num x)]
              (and (not= x diff) (contains? preamble-set diff))))
          preamble-set)))


(defn find-invalid-number [numbers preamble-size]
  (loop [i preamble-size
         preamble (take preamble-size numbers)
         remaining (drop preamble-size numbers)]
    (if (empty? remaining)
      nil ; No invalid number found (shouldn't happen in real input)
      (let [next-num (first remaining)]
        (if (valid-next-number? preamble next-num)
          (recur (inc i) (conj (vec (rest preamble)) next-num) (rest remaining))
          next-num)))))


(defn -main []
  (let [input-str (slurp "input.txt")
        numbers (map parse-long (str/split-lines input-str))
        preamble-size 25]
    (println (find-invalid-number numbers preamble-size))))

(-main)
