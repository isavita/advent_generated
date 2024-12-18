
(ns transparent-origami
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[dots-str folds-str] (str/split input #"\n\n")]
    {:dots (->> (str/split-lines dots-str)
                (map #(str/split % #","))
                (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]))
                set)
     :folds (->> (str/split-lines folds-str)
                 (map #(re-find #"fold along (x|y)=(\d+)" %))
                 (map (fn [[_ axis val]] [(keyword axis) (Integer/parseInt val)])))}))

(defn fold-point [[x y] [axis val]]
  (case axis
    :x (if (> x val) [(- (* 2 val) x) y] [x y])
    :y (if (> y val) [x (- (* 2 val) y)] [x y])))

(defn apply-fold [{:keys [dots] :as paper} fold]
  (assoc paper :dots (->> dots
                          (map #(fold-point % fold))
                          set)))

(defn solve [input]
  (let [{:keys [dots folds]} (parse-input input)
        first-fold (first folds)
        folded-paper (apply-fold {:dots dots} first-fold)]
    (count (:dots folded-paper))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
