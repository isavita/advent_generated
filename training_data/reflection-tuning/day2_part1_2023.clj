(ns cube-conundrum
  (:require [clojure.string :as str]))

(def max-cubes {"red" 12, "green" 13, "blue" 14})

(defn parse-game [line]
  (let [[_ id sets] (re-find #"Game (\d+): (.*)" line)
        parse-set (fn [set]
                    (->> (str/split set #", ")
                         (map #(let [[n color] (str/split % #" ")]
                                 [color (Integer/parseInt n)]))
                         (into {})))]
    {:id (Integer/parseInt id)
     :sets (map parse-set (str/split sets #"; "))}))

(defn possible-game? [game]
  (every? (fn [set]
            (every? (fn [[color count]]
                      (<= count (get max-cubes color)))
                    set))
          (:sets game)))

(defn solve-puzzle [input]
  (->> input
       str/split-lines
       (map parse-game)
       (filter possible-game?)
       (map :id)
       (reduce +)))

(defn -main []
  (let [input (slurp "input.txt")
        result (solve-puzzle input)]
    (println result)))

(-main)
