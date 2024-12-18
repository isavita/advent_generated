
(ns supply-stacks
  (:require [clojure.string :as str]))

(defn parse-stacks [lines]
  (let [stack-lines (take-while #(not (str/blank? %)) lines)
        num-stacks (-> (last stack-lines)
                       (str/trim)
                       (str/split #" +")
                       (last)
                       (Integer/parseInt))
        stacks (vec (repeat num-stacks []))]
    (reduce (fn [stacks line]
              (loop [i 0
                     stacks stacks]
                (if (>= i num-stacks)
                  stacks
                  (let [crate (get line (+ 1 (* 4 i)))]
                    (if (and crate (not (= \space crate)))
                      (recur (inc i) (assoc stacks i (conj (get stacks i) crate)))
                      (recur (inc i) stacks))))))
            stacks
            (butlast stack-lines))))

(defn parse-moves [lines]
  (->> lines
       (drop-while #(not (str/starts-with? % "move")))
       (map #(re-seq #"\d+" %))
       (map (fn [matches]
              (map #(Integer/parseInt %) matches)))))

(defn move-crates [stacks [count from to] move-multiple?]
  (let [from-idx (dec from)
        to-idx (dec to)
        crates-to-move (take count (get stacks from-idx))
        remaining-from (drop count (get stacks from-idx))
        moved-crates (if move-multiple? crates-to-move (reverse crates-to-move))]
    (-> stacks
        (assoc from-idx remaining-from)
        (assoc to-idx (concat moved-crates (get stacks to-idx))))))

(defn solve [input move-multiple?]
  (let [[stack-lines move-lines] (split-with #(not (str/starts-with? % "move")) input)
        stacks (parse-stacks stack-lines)
        moves (parse-moves move-lines)
        final-stacks (reduce (fn [stacks move] (move-crates stacks move move-multiple?)) stacks moves)]
    (->> final-stacks
         (map first)
         (str/join))))

(defn -main []
  (let [input (-> "input.txt"
                  slurp
                  str/split-lines)]
    (println "Part 1:" (solve input false))
    (println "Part 2:" (solve input true))))

(-main)
