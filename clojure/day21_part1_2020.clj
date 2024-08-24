(ns allergen-assessment
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[_ ingredients allergens] (re-matches #"(.+) \(contains (.+)\)" line)]
    [(set (str/split ingredients #"\s")) (set (str/split allergens #",\s"))]))

(defn read-input [filename]
  (with-open [rdr (io/reader filename)]
    (doall (map parse-line (line-seq rdr)))))

(defn solve []
  (let [foods (read-input "input.txt")
        ingredient-counts (apply merge-with + (map (fn [[ingredients _]] (frequencies ingredients)) foods))
        allergen-to-ingredients (reduce (fn [m [ingredients allergens]]
                                          (reduce (fn [m allergen]
                                                    (update m allergen (fn [s] (if s (set/intersection s ingredients) ingredients))))
                                                  m allergens))
                                        {} foods)
        allergenic-ingredients (apply set/union (vals allergen-to-ingredients))
        safe-ingredients (set/difference (set (keys ingredient-counts)) allergenic-ingredients)]
    (apply + (map #(get ingredient-counts % 0) safe-ingredients))))

(println (solve))