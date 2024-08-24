(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input []
  (with-open [r (io/reader "input.txt")]
    (let [instructions (str/trim (first (line-seq r)))
          nodes (into {} (map (fn [line]
                                (let [[node rest] (str/split line #" = ")]
                                  [node (str/split (subs rest 1 (dec (count rest))) #", ")]))
                              (rest (line-seq r))))]
      [instructions nodes])))

(defn follow-instructions [instructions nodes]
  (loop [current "AAA"
         steps 0
         index 0]
    (if (= current "ZZZ")
      steps
      (let [next (if (= (nth instructions index) \R)
                   (second (get nodes current))
                   (first (get nodes current)))]
        (recur next (inc steps) (mod (inc index) (count instructions)))))))

(let [[instructions nodes] (read-input)]
  (println (follow-instructions instructions nodes)))