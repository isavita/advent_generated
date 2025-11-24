
(ns aoc
  (:require [clojure.string :as str]))

(defn parse-workflow [line]
  (let [[_ name rules-str] (re-matches #"(\w+)\{(.*)\}" line)
        rules (->> (str/split rules-str #",")
                   (mapv (fn [rule]
                           (if-let [[_ cat op num dest] (re-matches #"([xmas])([<>])(\d+):(\w+)" rule)]
                             {:cat (first cat) :op (first op) :num (parse-long num) :dest dest}
                             {:dest rule}))))]
    [name rules]))

(defn parse-part [line]
  (into {} (map (fn [[_ k v]] [(first k) (parse-long v)])
                (re-seq #"([xmas])=(\d+)" line))))

(defn apply-rule [part {:keys [cat op num dest]}]
  (if (nil? cat) dest
      (let [v (get part cat)]
        (cond
          (= op \>) (when (> v num) dest)
          (= op \<) (when (< v num) dest)))))

(defn process-part [part workflows]
  (loop [curr "in"]
    (cond
      (= curr "A") true
      (= curr "R") false
      :else (let [rules (get workflows curr)]
              (if-let [dest (some #(apply-rule part %) rules)]
                (recur dest)
                (recur (get (last rules) :dest)))))))

(defn solve [input]
  (let [chunks (partition-by empty? input)
        workflows (into {} (map parse-workflow) (first chunks))
        parts (map parse-part (nth chunks 2))]
    (->> parts
         (filter #(process-part % workflows))
         (mapcat vals)
         (reduce +))))

(-> (slurp "input.txt")
    (str/split-lines)
    solve
    println)
