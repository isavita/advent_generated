(ns day4.core
  (:require [clojure.string :as str]))

(defn parse-room [line]
  (let [[_ name sector checksum] (re-find #"([a-z-]+)-(\d+)\[([a-z]+)\]" line)]
    {:name (str/replace name "-" "")
     :sector (Integer/parseInt sector)
     :checksum checksum}))

(defn calculate-checksum [name]
  (->> (frequencies name)
       (sort-by (juxt (comp - val) key))
       (take 5)
       (map first)
       (apply str)))

(defn real-room? [{:keys [name checksum]}]
  (= checksum (calculate-checksum name)))

(defn solve-part1 [input]
  (->> input
       (map parse-room)
       (filter real-room?)
       (map :sector)
       (reduce +)))

(defn -main []
  (let [input (str/split-lines (slurp "input.txt"))
        result (solve-part1 input)]
    (println result)))

(-main)
