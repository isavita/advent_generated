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

(defn rotate-row [screen y amount]
  (update screen y #(vec (take width (drop (- width (mod amount width)) (cycle %))))))

(defn rotate-column [screen x amount]
  (let [column (map #(nth % x) screen)
        rotated (take height (drop (- height (mod amount height)) (cycle column)))]
    (mapv #(assoc %1 x %2) screen rotated)))

(defn parse-instruction [instruction]
  (let [[op & args] (str/split instruction #" ")]
    (case op
      "rect" (let [[w h] (map #(Integer/parseInt %) (str/split (first args) #"x"))]
               #(rect % w h))
      "rotate" (let [axis (second args)
                     n (Integer/parseInt (re-find #"\d+" (nth args 2)))
                     amount (Integer/parseInt (last args))]
                 (if (= axis "row")
                   #(rotate-row % n amount)
                   #(rotate-column % n amount))))))

(defn apply-instructions [instructions]
  (reduce (fn [screen instruction]
            ((parse-instruction instruction) screen))
          (create-screen)
          instructions))

(defn count-lit-pixels [screen]
  (count (filter identity (flatten screen))))

(defn display-screen [screen]
  (doseq [row screen]
    (println (apply str (map #(if % "#" ".") row)))))

(defn solve-part1 [input]
  (let [instructions (str/split-lines input)
        final-screen (apply-instructions instructions)]
    (count-lit-pixels final-screen)))

(defn solve-part2 [input]
  (let [instructions (str/split-lines input)
        final-screen (apply-instructions instructions)]
    (display-screen final-screen)
    "Read the displayed message"))

;; Example usage:
;; (def input (slurp "input.txt"))
;; (println "Part 1:" (solve-part1 input))
;; (println "Part 2:" (solve-part2 input))
