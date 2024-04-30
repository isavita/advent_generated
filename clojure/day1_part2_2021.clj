(ns sonar-sweep
  (:require [clojure.java.io :as io]))

(defn read-input []
  (with-open [r (io/reader "input.txt")]
    (doall (map #(Integer/parseInt %) (line-seq r)))))

(defn part-1 [depths]
  (count (filter pos? (map - (rest depths) depths))))

(defn part-2 [depths]
  (let [windows (partition 3 1 depths)
        sums (map #(apply + %) windows)]
    (count (filter pos? (map - (rest sums) sums)))))

(defn -main []
  (let [depths (read-input)]
    (println "Part 1:" (part-1 depths))
    (println "Part 2:" (part-2 depths))))

(-main)