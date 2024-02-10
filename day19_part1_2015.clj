
(ns solution
  (:require [clojure.java.io :as io]))

(defn read-lines [filename]
  (with-open [reader (io/reader filename)]
    (doall (line-seq reader))))

(defn main []
  (let [lines (read-lines "input.txt")
        replacements (atom [])
        molecule (atom "")]
    (doseq [line lines]
      (if (not= line "")
        (if (clojure.string/includes? line " => ")
          (swap! replacements conj line)
          (reset! molecule line))))
    
    (def molecules (atom {}))
    (doseq [replacement @replacements]
      (let [parts (clojure.string/split replacement #" => ")]
        (dotimes [i (count @molecule)]
          (if (clojure.string/starts-with? (subs @molecule i) (first parts))
            (let [new-molecule (str (subs @molecule 0 i) (second parts) (subs @molecule (+ i (count (first parts)))))]
              (swap! molecules assoc new-molecule true))))))
    
    (println (count @molecules))))

(main)
