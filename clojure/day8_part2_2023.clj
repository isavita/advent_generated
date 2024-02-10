
(ns solution
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[head children] (str/split line #" = ")]
    (let [children-trim (str/trim (subs children 1 (dec (count children))))]
      [head (str/split children-trim #", ")])))

(defn gcd [a b]
  (loop [a a b b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcm-slice [nums]
  (if (empty? nums)
    0
    (reduce lcm nums)))

(defn solve [input]
  (let [instructions (first input)
        nodes (reduce (fn [acc line]
                        (let [[head children] (parse-line line)]
                          (assoc acc head children))) {} (drop 2 input))
        starts (filter #(= (last %) \A) (keys nodes))
        instructions-length (count instructions)
        steps (map (fn [start]
                     (loop [element start steps 0]
                       (if (not= (last element) \Z)
                         (let [instruction (get instructions (mod steps instructions-length))]
                           (recur (if (= instruction \L) (first (get nodes element)) (second (get nodes element))) (inc steps)))
                         steps))) starts)]
    (lcm-slice steps)))

(defn read-file [file-name]
  (-> (slurp file-name)
      (str/split-lines)))

(def input (read-file "input.txt"))
(println (solve input))
