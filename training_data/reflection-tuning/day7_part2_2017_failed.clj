(ns day7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[_ name weight children] (re-matches #"(\w+) \((\d+)\)(?: -> ([\w, ]+))?" line)]
    {:name name
     :weight (Integer/parseInt weight)
     :children (when children (str/split children #", "))}))

(defn build-tree [input]
  (let [nodes (map parse-line input)
        node-map (into {} (map (juxt :name identity) nodes))
        root (first (set/difference
                      (set (map :name nodes))
                      (set (mapcat :children nodes))))]
    (assoc node-map root (assoc (node-map root) :root true))))

(defn total-weight [tree node]
  (let [{:keys [weight children]} (tree node)]
    (+ weight (apply + (map #(total-weight tree %) children)))))

(defn find-unbalanced [tree node]
  (let [{:keys [children]} (tree node)
        child-weights (map #(total-weight tree %) children)]
    (if (apply = child-weights)
      nil
      (let [weight-freq (frequencies child-weights)
            [unbalanced-weight balanced-weight] (map first (sort-by (comp count second) weight-freq))
            unbalanced-child (first (filter #(= unbalanced-weight (total-weight tree %)) children))
            child-result (find-unbalanced tree unbalanced-child)]
        (or child-result
            {:node unbalanced-child
             :current-weight (:weight (tree unbalanced-child))
             :needed-weight (+ (:weight (tree unbalanced-child))
                               (- balanced-weight unbalanced-weight))})))))

(defn solve [input]
  (let [tree (build-tree input)
        root (first (filter #(:root (val %)) tree))
        unbalanced (find-unbalanced tree (key root))]
    (:needed-weight unbalanced)))

(def input (str/split-lines (slurp "input.txt")))
(println (solve input))
