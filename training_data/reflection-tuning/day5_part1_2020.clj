(ns day5.core
  (:require [clojure.string :as str]))

(defn boarding-pass-to-seat-id [pass]
  (Integer/parseInt
   (-> pass
       (str/replace #"[FL]" "0")
       (str/replace #"[BR]" "1"))
   2))

(defn highest-seat-id [filename]
  (->> (slurp filename)
       str/split-lines
       (map boarding-pass-to-seat-id)
       (apply max)))

(defn -main []
  (println (highest-seat-id "input.txt")))

(-main)
