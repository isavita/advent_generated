(ns mode-maze
  (:require [clojure.java.io :as io]))

(defn calculate-risk-level [depth target-x target-y]
  (let [geologic-index (atom {})
        erosion-level (atom {})]
    (doseq [x (range (inc target-x))
            y (range (inc target-y))]
      (let [gi (cond
                  (and (= x 0) (= y 0)) 0
                  (and (= x target-x) (= y target-y)) 0
                  (= y 0) (* x 16807)
                  (= x 0) (* y 48271)
                  :else (* (@erosion-level [(dec x) y]) (@erosion-level [x (dec y)])))]
        (swap! geologic-index assoc [x y] gi)
        (let [el (mod (+ gi depth) 20183)]
          (swap! erosion-level assoc [x y] el))))
    (reduce + (for [x (range (inc target-x))
                    y (range (inc target-y))]
                (mod (@erosion-level [x y]) 3)))))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [depth (Integer/parseInt (clojure.string/replace (.readLine r) "depth: " ""))
          target (clojure.string/split (clojure.string/replace (.readLine r) "target: " "") #",")
          target-x (Integer/parseInt (first target))
          target-y (Integer/parseInt (second target))]
      (println (calculate-risk-level depth target-x target-y)))))

(-main)