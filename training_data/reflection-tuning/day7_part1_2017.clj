(ns day7.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[name weight & supported] (str/split line #"[(),\s->]+")]
    {:name name
     :weight (Integer/parseInt weight)
     :supported (set supported)}))

(defn find-bottom-program [programs]
  (let [all-programs (set (map :name programs))
        supported-programs (set (mapcat :supported programs))]
    (first (set/difference all-programs supported-programs))))

(defn solve-puzzle []
  (let [input (slurp "input.txt")
        programs (map parse-line (str/split-lines input))
        bottom-program (find-bottom-program programs)]
    (println bottom-program)))

(solve-puzzle)
