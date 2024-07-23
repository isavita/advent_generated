
(ns digital-plumber
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [filename]
  (with-open [rdr (io/reader filename)]
    (reduce (fn [acc line]
              (let [[program connections] (str/split line #"<->")
                    program-id (Integer/parseInt (str/trim program))
                    connected-ids (map #(Integer/parseInt (str/trim %)) (str/split (second (str/split line #"<->")) #","))]
                (assoc acc program-id connected-ids)))
            {}
            (line-seq rdr))))

(defn build-graph [input]
  (reduce (fn [graph [program connections]]
            (assoc graph program (set connections)))
          {}
          input))

(defn dfs [graph visited node]
  (if (visited node)
    visited
    (reduce (fn [v n] (dfs graph v n)) (assoc visited node true) (graph node))))

(defn count-programs-in-group [graph start]
  (count (keys (dfs graph {} start))))

(defn -main []
  (let [input (parse-input "input.txt")
        graph (build-graph input)]
    (println "Number of programs in the group containing program ID 0:" (count-programs-in-group graph 0))))

(-main)
