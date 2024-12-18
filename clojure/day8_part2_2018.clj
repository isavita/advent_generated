
(ns day8
  (:require [clojure.string :as str]))

(defn parse-node [numbers]
  (let [[num-children num-metadata & rest] numbers
        children (loop [remaining rest
                        child-nodes []
                        count 0]
                   (if (= count num-children)
                     [child-nodes remaining]
                     (let [[child-node-data next-remaining] (parse-node remaining)]
                       (recur next-remaining (conj child-nodes child-node-data) (inc count)))))
        [child-nodes remaining] children
        metadata (take num-metadata remaining)
        remaining (drop num-metadata remaining)]
    [{:children child-nodes :metadata metadata} remaining]))

(defn sum-metadata [node]
  (let [{:keys [children metadata]} node]
    (+ (reduce + metadata)
       (reduce + (map sum-metadata children)))))

(defn node-value [node]
  (let [{:keys [children metadata]} node]
    (if (empty? children)
      (reduce + metadata)
      (reduce + (map (fn [index]
                       (if (and (pos? index) (<= index (count children)))
                         (node-value (nth children (dec index)))
                         0))
                     metadata)))))

(defn solve [input]
  (let [numbers (map #(Integer/parseInt %) (str/split input #" "))
        [root-node _] (parse-node numbers)]
    [(sum-metadata root-node) (node-value root-node)]))

(defn -main []
  (let [input (slurp "input.txt")
        [metadata-sum root-value] (solve input)]
    (println "Sum of metadata:" metadata-sum)
    (println "Value of root node:" root-value)))

(-main)
