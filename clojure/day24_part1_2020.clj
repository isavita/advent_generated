(ns lobby-layout
  (:require [clojure.java.io :as io]))

(def directions {"e" [1 0] "se" [0 1] "sw" [-1 1] "w" [-1 0] "nw" [0 -1] "ne" [1 -1]})

(defn parse-directions [line]
  (loop [remaining line
         result []]
    (if (empty? remaining)
      result
      (let [dir (if (contains? directions (subs remaining 0 1))
                  (subs remaining 0 1)
                  (subs remaining 0 2))]
        (recur (subs remaining (count dir)) (conj result dir))))))

(defn flip-tile [tile]
  (reduce (fn [[x y] dir]
            (let [[dx dy] (get directions dir)]
              [(+ x dx) (+ y dy)]))
          [0 0]
          (parse-directions tile)))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [black-tiles (reduce (fn [tiles tile]
                                (let [coord (flip-tile tile)]
                                  (if (contains? tiles coord)
                                    (disj tiles coord)
                                    (conj tiles coord))))
                              #{}
                              (line-seq rdr))]
      (println (count black-tiles)))))

(-main)