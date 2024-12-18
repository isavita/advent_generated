
(ns two-factor-auth
  (:require [clojure.string :as str]))

(def width 50)
(def height 6)

(defn create-screen []
  (vec (repeat height (vec (repeat width false)))))

(defn rect [screen a b]
  (mapv (fn [row-idx row]
          (if (< row-idx b)
            (mapv (fn [col-idx pixel]
                    (if (< col-idx a) true pixel))
                  (range width) row)
            row))
        (range height) screen))

(defn rotate-row [screen y b]
  (let [row (nth screen y)]
    (assoc screen y (vec (concat (drop (- (count row) b) row) (take (- (count row) b) row))))))

(defn rotate-col [screen x b]
  (let [col (mapv #(nth % x) screen)
        rotated-col (vec (concat (drop (- (count col) b) col) (take (- (count col) b) col)))]
    (mapv (fn [row-idx row]
            (assoc row x (nth rotated-col row-idx)))
          (range height) screen)))

(defn parse-instruction [instruction]
  (cond
    (str/starts-with? instruction "rect")
    (let [[_ a b] (re-matches #"rect (\d+)x(\d+)" instruction)]
      {:op :rect :a (Integer/parseInt a) :b (Integer/parseInt b)})
    (str/starts-with? instruction "rotate row")
    (let [[_ y b] (re-matches #"rotate row y=(\d+) by (\d+)" instruction)]
      {:op :rotate-row :y (Integer/parseInt y) :b (Integer/parseInt b)})
    (str/starts-with? instruction "rotate column")
    (let [[_ x b] (re-matches #"rotate column x=(\d+) by (\d+)" instruction)]
      {:op :rotate-col :x (Integer/parseInt x) :b (Integer/parseInt b)})
    :else nil))

(defn apply-instruction [screen instruction]
  (case (:op instruction)
    :rect (rect screen (:a instruction) (:b instruction))
    :rotate-row (rotate-row screen (:y instruction) (:b instruction))
    :rotate-col (rotate-col screen (:x instruction) (:b instruction))
    screen))

(defn count-lit-pixels [screen]
  (reduce + (map (fn [row] (count (filter identity row))) screen)))

(defn print-screen [screen]
  (doseq [row screen]
    (println (str/join "" (map #(if % "#" ".") row)))))

(defn solve [instructions]
  (let [screen (reduce apply-instruction (create-screen) instructions)
        lit-count (count-lit-pixels screen)]
    (println "Part 1: Lit pixels:" lit-count)
    (println "Part 2: Screen output:")
    (print-screen screen)))

(defn -main [& args]
  (let [instructions (-> "input.txt"
                         slurp
                         str/split-lines
                         (->> (map parse-instruction)))]
    (solve instructions)))

(-main)
