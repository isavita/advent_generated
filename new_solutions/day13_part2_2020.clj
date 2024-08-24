(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input [filename]
  (with-open [rdr (io/reader filename)]
    (let [lines (line-seq rdr)]
      [(Integer/parseInt (first lines))
       (str/split (second lines) #",")])))

(defn part-one [earliest-timestamp bus-ids]
  (let [buses (filter #(not= % "x") bus-ids)
        buses (map #(Integer/parseInt %) buses)
        min-wait (apply min (map #(let [wait (- % (mod earliest-timestamp %))]
                                    (if (= wait %) 0 wait)) buses))
        earliest-bus (first (filter #(= (- % (mod earliest-timestamp %)) min-wait) buses))]
    (* earliest-bus min-wait)))

(defn part-two [bus-ids]
  (let [buses (map-indexed (fn [idx id]
                             (when (not= id "x")
                               [(Integer/parseInt id) idx])) bus-ids)
        buses (filter some? buses)]
    (loop [t 0 step 1 buses buses]
      (if (empty? buses)
        t
        (let [[id offset] (first buses)]
          (if (= 0 (mod (+ t offset) id))
            (recur t (* step id) (rest buses))
            (recur (+ t step) step buses)))))))

(defn -main []
  (let [[earliest-timestamp bus-ids] (read-input "input.txt")]
    (println "Part One:" (part-one earliest-timestamp bus-ids))
    (println "Part Two:" (part-two bus-ids))))

(-main)