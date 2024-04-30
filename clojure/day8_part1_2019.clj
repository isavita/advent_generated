(ns day8
  (:gen-class))

(defn read-input []
  (slurp "input.txt"))

(defn parse-input [input]
  (map #(Integer/parseInt %) (map str input)))

(defn layerize [width height pixels]
  (partition (* width height) pixels))

(defn count-digits [layer digit]
  (count (filter #(= % digit) layer)))

(defn solve []
  (let [input (parse-input (read-input))
        width 25
        height 6
        layers (layerize width height input)]
    (let [min-zero-layer (apply min-key #(count-digits % 0) layers)]
      (* (count-digits min-zero-layer 1) (count-digits min-zero-layer 2)))))

(println (solve))