(ns crab-cups
  (:require [clojure.string :as str]))

(defn read-input []
  (slurp "input.txt"))

(defn play-game [cups moves]
  (let [max-cup (apply max cups)
        cup-map (reduce (fn [m [a b]] (assoc m a b))
                        {}
                        (partition 2 1 (concat cups [(first cups)])))]
    (loop [current (first cups)
           cup-map cup-map
           moves moves]
      (if (zero? moves)
        cup-map
        (let [pickup1 (cup-map current)
              pickup2 (cup-map pickup1)
              pickup3 (cup-map pickup2)
              next-cup (cup-map pickup3)
              destination (loop [dest (dec current)]
                            (if (or (< dest 1) (#{pickup1 pickup2 pickup3} dest))
                              (recur (if (< dest 1) max-cup (dec dest)))
                              dest))]
          (recur next-cup
                 (-> cup-map
                     (assoc current next-cup)
                     (assoc pickup3 (cup-map destination))
                     (assoc destination pickup1))
                 (dec moves)))))))

(defn cups-after-one [cup-map]
  (loop [result ""
         current (cup-map 1)]
    (if (= current 1)
      result
      (recur (str result current) (cup-map current)))))

(defn -main []
  (let [cups (map #(Integer/parseInt (str %)) (str/split (read-input) #""))]
    (-> (play-game cups 100)
        cups-after-one
        println)))

(-main)