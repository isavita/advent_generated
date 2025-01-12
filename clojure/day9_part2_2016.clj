
(ns decompressed-length
  (:require [clojure.string :as str]))

(defn decompress [input start end]
  (let [marker-regex #"\((\d+)x(\d+)\)"
        length (atom 0)
        i (atom start)]
    (while (< @i end)
      (if-let [marker (re-find marker-regex (subs input @i end))]
        (let [char-count (Integer/parseInt (nth marker 1))
              repeat-count (Integer/parseInt (nth marker 2))
              next-index (+ @i (count (first marker)))]
          (swap! length + (* repeat-count (decompress input next-index (+ next-index char-count))))
          (reset! i (+ next-index char-count)))
        (do
          (swap! length inc)
          (swap! i inc))))
    @length))

(defn get-decompressed-length-v2 [input]
  (decompress input 0 (count input)))

(let [input (slurp "input.txt")]
  (println (get-decompressed-length-v2 input)))
