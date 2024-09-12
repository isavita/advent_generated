(ns day8
  (:require [clojure.string :as str]))

(def width 25)
(def height 6)

(defn parse-input [input]
  (mapv #(Character/digit % 10) input))

(defn split-layers [pixels]
  (partition (* width height) pixels))

(defn count-digit [layer digit]
  (count (filter #(= % digit) layer)))

(defn part1 [input]
  (let [pixels (parse-input input)
        layers (split-layers pixels)
        layer-with-fewest-zeros (apply min-key #(count-digit % 0) layers)]
    (* (count-digit layer-with-fewest-zeros 1)
       (count-digit layer-with-fewest-zeros 2))))

(defn combine-pixels [pixels]
  (reduce (fn [acc pixel]
            (if (= acc 2) pixel acc))
          2
          pixels))

(defn combine-layers [layers]
  (apply map combine-pixels layers))

(defn render-image [final-layer]
  (->> final-layer
       (partition width)
       (map #(map (fn [pixel] (case pixel
                                0 "█"  ; Black pixel
                                1 " "  ; White pixel (space for better visibility)
                                2 "?")) %))  ; This shouldn't occur in the final image
       (map str/join)
       (str/join "\n")))

(defn part2 [input]
  (let [pixels (parse-input input)
        layers (split-layers pixels)
        final-layer (combine-layers layers)]
    (render-image final-layer)))

(defn solve [input]
  (println "Part 1:" (part1 input))
  (println "Part 2:\n" (part2 input)))

; Call (solve input-string) to get the solution
