
(ns day23.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-connections [input]
  (let [lines (str/split-lines input)
        connections (map #(str/split % #"-") lines)
        all-connections (reduce (fn [acc [a b]]
                                  (conj acc [a b] [b a]))
                                [] connections)]
    (reduce (fn [graph [a b]]
              (-> graph
                  (update a #(if (nil? %) #{b} (conj % b)))
                  (update b #(if (nil? %) #{a} (conj % a)))))
            {} all-connections)))

(defn find-triplets [graph]
  (let [nodes (keys graph)]
    (for [a nodes
          b (sort (vec (graph a)))
          :when (not= a b)
          c (sort (vec (graph b)))
          :when (and (not= a c) (not= b c) (contains? (graph a) c))]
      (sort [a b c]))))

(defn solve-part1 [graph]
  (->> (find-triplets graph)
       distinct
       (filter (fn [triplet]
                 (some #(str/starts-with? % "t") triplet)))
       count))

(defn find-cliques [graph]
  (let [nodes (vec (keys graph))]
    (loop [cliques #{}
           i 0]
      (if (>= i (count nodes))
        cliques
        (let [node (nth nodes i)
              neighbors (graph node)
              new-cliques (reduce (fn [acc clique]
                                    (if (set/subset? clique neighbors)
                                      (conj acc (conj clique node))
                                      acc))
                                  #{#{node}}
                                  cliques)]
           (recur (set/union cliques new-cliques) (inc i)))))))
  

(defn solve-part2 [graph]
    (let [all-cliques (find-cliques graph)
        max-clique-size (apply max (map count all-cliques))
        max-cliques (filter #(= (count %) max-clique-size) all-cliques)]
      
      (if (empty? max-cliques)
          ""
          (str/join "," (sort (first max-cliques))))))

(defn -main []
  (let [input (slurp "input.txt")
        graph (parse-connections input)]
    (println "Part 1:" (solve-part1 graph))
    (println "Part 2:" (solve-part2 graph))))

(-main)
