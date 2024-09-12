(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (mapv #(Integer/parseInt %))
       (sort)))

(defn find-two-sum [numbers target]
  (loop [i 0]
    (when (< i (dec (count numbers)))
      (let [a (nth numbers i)]
        (loop [j (inc i)]
          (if (< j (count numbers))
            (let [b (nth numbers j)]
              (cond
                (= (+ a b) target) (* a b)
                (> (+ a b) target) (recur (inc i))
                :else (recur (inc j))))
            (recur (inc i))))))))

(defn find-three-sum [numbers target]
  (loop [i 0]
    (when (< i (- (count numbers) 2))
      (let [a (nth numbers i)]
        (loop [j (inc i)]
          (if (< j (dec (count numbers)))
            (let [b (nth numbers j)]
              (loop [k (inc j)]
                (if (< k (count numbers))
                  (let [c (nth numbers k)]
                    (cond
                      (= (+ a b c) target) (* a b c)
                      (> (+ a b c) target) (recur (inc j))
                      :else (recur (inc k))))
                  (recur (inc j)))))
            (recur (inc i))))))))

(defn solve []
  (let [numbers (read-input "input.txt")]
    (println "Part One:" (find-two-sum numbers 2020))
    (println "Part Two:" (find-three-sum numbers 2020))))

(solve)
