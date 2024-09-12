(ns day8-solution
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[instructions nodes] (str/split input #"\n\n")
        node-map (into {} (map (fn [line]
                                 (let [[node left right] (re-seq #"[A-Z0-9]{3}" line)]
                                   [node [left right]]))
                               (str/split-lines nodes)))]
    [instructions node-map]))

(defn next-node [node-map node instruction]
  (let [direction (if (= instruction \L) 0 1)]
    (get-in node-map [node direction])))

(defn steps-to-end [instructions node-map start end?]
  (loop [node start
         steps 0
         instr-seq (cycle instructions)]
    (if (end? node)
      steps
      (recur (next-node node-map node (first instr-seq))
             (inc steps)
             (rest instr-seq)))))

(defn solve-part1 [input]
  (let [[instructions node-map] (parse-input input)]
    (steps-to-end instructions node-map "AAA" #(= % "ZZZ"))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [& numbers]
  (reduce (fn [a b]
            (/ (* a b) (gcd a b)))
          numbers))

(defn solve-part2 [input]
  (let [[instructions node-map] (parse-input input)
        start-nodes (filter #(.endsWith % "A") (keys node-map))
        end? #(.endsWith % "Z")
        cycle-lengths (map #(steps-to-end instructions node-map % end?) start-nodes)]
    (apply lcm cycle-lengths)))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))
