
(ns day13
  (:require [clojure.java.io :as io]))

(defn- is-open-space? [x y favorite-number]
  (let [num (+ (* x x) (* 3 x) (* 2 x y) y (* y y) favorite-number)
        binary (Integer/toBinaryString num)
        ones-count (count (filter #(= \1 %) binary))]
    (even? ones-count)))

(defn- neighbors [x y]
  (->> [[(inc x) y]
        [x (inc y)]
        [(dec x) y]
        [x (dec y)]]
       (filter (fn [[nx ny]] (and (>= nx 0) (>= ny 0))))))

(defn solve-part1 [favorite-number target-x target-y]
  (let [start-x 1
        start-y 1
        initial-state {:visited #{} :queue (conj clojure.lang.PersistentQueue/EMPTY [start-x start-y 0])}]

    (loop [{:keys [visited queue]} initial-state]
      (if (empty? queue)
        -1  ; Target not reachable
        (let [[x y steps] (peek queue)
              rest-queue (pop queue)]
          (if (and (= x target-x) (= y target-y))
            steps
            (if (visited [x y])
              (recur {:visited visited :queue rest-queue})
              (let [new-visited (conj visited [x y])
                    valid-neighbors (filter #(is-open-space? (first %) (second %) favorite-number) (neighbors x y))
                    new-queue (reduce conj rest-queue (map #(conj % (inc steps)) valid-neighbors))]
                (recur {:visited new-visited :queue new-queue})))))))))

(defn solve-part2 [favorite-number max-steps]
    (let [start-x 1
          start-y 1
          initial-state {:visited #{} :queue (conj clojure.lang.PersistentQueue/EMPTY [start-x start-y 0])}]
  
      (loop [{:keys [visited queue]} initial-state]
        (if (empty? queue)
          (count visited)
          (let [[x y steps] (peek queue)
                rest-queue (pop queue)]
            (if (or (visited [x y]) (> steps max-steps) )
              (recur {:visited visited :queue rest-queue})
              (let [new-visited (conj visited [x y])
                    valid-neighbors (filter #(is-open-space? (first %) (second %) favorite-number) (neighbors x y))
                    new-queue (reduce conj rest-queue (map #(conj % (inc steps)) (filter #(< (peek %) (inc max-steps)) valid-neighbors)))] ;optimized to check step limit
                (recur {:visited new-visited :queue new-queue}))))))))

(defn -main []
  (let [favorite-number (with-open [rdr (io/reader "input.txt")]
                          (parse-long (first (line-seq rdr))))]
    (println "Part 1:" (solve-part1 favorite-number 31 39))
    (println "Part 2:" (solve-part2 favorite-number 50))))

(-main)
