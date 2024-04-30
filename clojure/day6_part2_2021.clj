(ns lanternfish
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn sum [coll]
  (reduce + coll))

(def lantern-fish-counts (vec (repeat 9 0)))

(with-open [rdr (io/reader "input.txt")]
  (doseq [line (line-seq rdr)]
    (doseq [age (str/split line #",")]
      (let [age-count (Integer/parseInt age)]
        (alter-var-root #'lantern-fish-counts update age-count inc)))))

(dotimes [i 256]
  (let [new-lantern-fish (nth lantern-fish-counts 0)]
    (alter-var-root #'lantern-fish-counts (fn [v]
                                          (conj (subvec v 1) new-lantern-fish)))
    (alter-var-root #'lantern-fish-counts update 6 + new-lantern-fish)))

(println (sum lantern-fish-counts))