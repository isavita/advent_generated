
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-graph [file-path]
  (with-open [rdr (io/reader file-path)]
    (reduce (fn [adj line]
              (if-let [[_ src dsts] (re-matches #"(\S+): (.+)" (str/trim line))]
                (let [dsts (str/split dsts #"\s+")]
                  (assoc adj src dsts))
                adj))
            {} (line-seq rdr))))

(defn count-paths [start end adj]
  (let [memo (atom {})]
    (letfn [(dfs [current]
              (cond
                (= current end) 1
                (contains? @memo current) (@memo current)
                :else (let [total (reduce + (map dfs (get adj current [])))]
                        (swap! memo assoc current total)
                        total)))]
      (dfs start))))

(defn -main [& _]
  (let [adj (parse-graph "input.txt")
        s1 (* (count-paths "svr" "dac" adj)
              (count-paths "dac" "fft" adj)
              (count-paths "fft" "out" adj))
        s2 (* (count-paths "svr" "fft" adj)
              (count-paths "fft" "dac" adj)
              (count-paths "dac" "out" adj))]
    (println (+ s1 s2))))

(-main)
