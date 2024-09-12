(ns space-image-format
  (:require [clojure.string :as str]))

(defn parse-layers [input width height]
  (partition (* width height) input))

(defn count-digits [layer]
  (frequencies layer))

(defn solve [input width height]
  (let [layers (parse-layers input width height)
        layer-counts (map count-digits layers)
        min-zeros-layer (apply min-key #(get % \0 0) layer-counts)
        ones (get min-zeros-layer \1 0)
        twos (get min-zeros-layer \2 0)]
    (* ones twos)))

(defn -main []
  (let [input (str/trim (slurp "input.txt"))
        width 25
        height 6
        result (solve input width height)]
    (println result)))

(-main)
