(ns supply-stacks
  (:require [clojure.string :as str]))

(defn parse-stacks [lines]
  (let [stack-count (-> lines last (str/split #"\s+") count dec)
        stacks (vec (repeat stack-count []))]
    (->> lines
         (drop-last)
         (reduce (fn [acc line]
                   (map-indexed (fn [i stack]
                                  (if-let [crate (get line (inc (* i 4)))]
                                    (if (Character/isLetter crate)
                                      (conj stack crate)
                                      stack)
                                    stack))
                                acc))
                 stacks)
         (mapv vec))))

(defn parse-move [line]
  (let [[_ count from to] (re-find #"move (\d+) from (\d+) to (\d+)" line)]
    [(Integer/parseInt count) (dec (Integer/parseInt from)) (dec (Integer/parseInt to))]))

(defn move-crates [stacks [count from to]]
  (let [moving-crates (take count (get stacks from))
        new-from (update stacks from #(drop count %))
        new-to (update new-from to #(concat (reverse moving-crates) %))]
    new-to))

(defn solve-supply-stacks [input]
  (let [[stacks-input moves-input] (split-with (complement str/blank?) input)
        initial-stacks (parse-stacks stacks-input)
        moves (map parse-move (rest moves-input))
        final-stacks (reduce move-crates initial-stacks moves)]
    (apply str (map first final-stacks))))

(defn -main []
  (let [input (str/split-lines (slurp "input.txt"))]
    (println (solve-supply-stacks input))))

(-main)
