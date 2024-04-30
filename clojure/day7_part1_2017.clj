(ns program
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn main []
  (let [lines (str/split (str/trim (slurp "input.txt")) #"\n")
        holder-map (reduce (fn [m line]
                             (let [names (re-seq #"[a-z]+" line)
                                   holder (first names)]
                               (assoc m holder true)))
                           {} lines)
        held-map (reduce (fn [m line]
                           (let [names (re-seq #"[a-z]+" line)]
                             (reduce (fn [m name]
                                       (assoc m name true))
                                     m (rest names))))
                         {} lines)]
    (doseq [holder (keys holder-map)]
      (when-not (held-map holder)
        (println holder)
        (System/exit 0)))))

(main)