(ns day8
  (:require [clojure.string :as str]))

(def width 50)
(def height 6)

(defn create-screen []
  (vec (repeat height (vec (repeat width false)))))

(defn rect [screen w h]
  (reduce (fn [s [y x]]
            (assoc-in s [y x] true))
          screen
          (for [y (range h) x (range w)] [y x])))

(defn rotate-row [screen row amount]
  (update screen row #(vec (take width (drop (- width (mod amount width)) (cycle %))))))

(defn rotate-column [screen col amount]
  (let [column (map #(nth % col) screen)
        rotated (take height (drop (- height (mod amount height)) (cycle column)))]
    (mapv #(assoc %1 col %2) screen rotated)))

(defn parse-instruction [instruction]
  (let [[cmd & args] (str/split instruction #" ")]
    (case cmd
      "rect" (let [[w h] (map #(Integer/parseInt %) (str/split (first args) #"x"))]
               [:rect w h])
      "rotate" (let [axis (second args)
                     [coord amount] (map #(Integer/parseInt (re-find #"\d+" %))
                                         (drop 2 args))]
                 (if (= axis "row")
                   [:rotate-row coord amount]
                   [:rotate-column coord amount]))
      (throw (IllegalArgumentException. (str "Unknown command: " cmd))))))

(defn apply-instruction [screen instruction]
  (let [[cmd & args] (parse-instruction instruction)]
    (case cmd
      :rect (apply rect screen args)
      :rotate-row (apply rotate-row screen args)
      :rotate-column (apply rotate-column screen args))))

(defn count-lit-pixels [screen]
  (count (filter true? (flatten screen))))

(defn process-instructions [instructions]
  (let [final-screen (reduce apply-instruction (create-screen) instructions)]
    (count-lit-pixels final-screen)))

(defn solve [input]
  (let [instructions (str/split-lines input)]
    (process-instructions instructions)))

;; Example usage:
;; (solve (slurp "input.txt"))
