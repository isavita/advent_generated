
(ns solution
  (:require [clojure.string :as str]))

(defn first-n-unique [s n]
  (loop [i n]
    (if (>= i (count s))
      -1
      (let [b (subs s (- i n) i)
            unique? (= (count b) (count (distinct b)))]
        (if unique?
          i
          (recur (inc i)))))))

(defn read-all [path]
  (-> (slurp path)
      (str/trim)))

(defn set-of [b]
  (into #{} b))

(defn -main []
  (let [s (read-all "input.txt")]
    (println (first-n-unique s 14))))

(-main)
