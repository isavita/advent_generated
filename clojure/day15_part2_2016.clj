
(ns timing-is-everything
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (when-let [[_ n-pos start] (re-matches #"Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)\." line)]
    [(Long/parseLong n-pos) (Long/parseLong start)]))

(defn discs []
  (let [base (with-open [rdr (io/reader "input.txt")]
               (->> (line-seq rdr)
                    (keep parse-line)
                    vec))]
    (conj base [11 0])))

(defn aligned? [discs t]
  (loop [i 0]
    (if (= i (count discs))
      true
      (let [[n-pos start] (nth discs i)]
        (if (zero? (mod (+ start t i 1) n-pos))
          (recur (inc i))
          false)))))

(defn -main []
  (let [discs (discs)]
    (println (first (drop-while #(not (aligned? discs %)) (range))))))

(-main)
