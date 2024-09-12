(ns day7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(re-seq #"\b[A-Z]\b" %))
       (reduce (fn [acc [prereq step]]
                 (-> acc
                     (update prereq (fnil conj #{}) step)
                     (update step (fnil conj #{})))) {})))

(defn topological-sort [graph]
  (loop [result []
         remaining-nodes (into (sorted-set) (keys graph))
         graph graph]
    (if (empty? remaining-nodes)
      result
      (let [available (set/difference remaining-nodes
                                      (set (mapcat second graph)))]
        (if (empty? available)
          (throw (Exception. "Cycle detected in graph"))
          (let [next-node (first available)]
            (recur (conj result next-node)
                   (disj remaining-nodes next-node)
                   (dissoc graph next-node))))))))

(defn part1 [input]
  (apply str (topological-sort (parse-input input))))

(defn step-time [step]
  (+ 61 (- (int (first step)) (int \A))))

(defn part2 [input workers]
  (let [graph (parse-input input)
        total-steps (count (keys graph))]
    (loop [time 0
           available (into (sorted-set) (keys graph))
           in-progress {}
           completed #{}]
      (if (= (count completed) total-steps)
        time
        (let [finished (filter (fn [[_ end-time]] (<= end-time time)) in-progress)
              newly-completed (into completed (map first finished))
              still-in-progress (apply dissoc in-progress (map first finished))
              newly-available (set/difference
                               (into available (mapcat #(get graph % []) (map first finished)))
                               newly-completed
                               (set (keys still-in-progress)))
              can-start (- workers (count still-in-progress))
              to-start (take can-start newly-available)
              new-in-progress (merge still-in-progress
                                     (zipmap to-start
                                             (repeat (+ time (step-time (first to-start))))))]
          (recur (inc time)
                 (apply disj newly-available to-start)
                 new-in-progress
                 newly-completed))))))

;; Example usage:
(def sample-input
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(println "Part 1:" (part1 sample-input))
(println "Part 2:" (part2 sample-input 2))
