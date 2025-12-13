
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-graph [file]
  (with-open [rdr (io/reader file)]
    (reduce (fn [acc line]
              (let [[src dsts] (str/split line #":\s*")]
                (assoc acc src (str/split dsts #"\s+"))))
            {} (remove str/blank? (line-seq rdr)))))

(defn count-paths [current target adj memo]
  (if (= current target)
    1
    (if-let [cached (@memo current)]
      cached
      (let [total (reduce + 0 (map #(count-paths % target adj memo) (get adj current [])))]
        (swap! memo assoc current total)
        total))))

(defn -main [& _]
  (let [adj (parse-graph "input.txt")
        memo (atom {})]
    (println (count-paths "you" "out" adj memo))))

(-main)
