
(ns bag-counter
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-rules [lines]
  (reduce (fn [acc line]
            (let [[_ bag contents] (re-find #"(.+) bags contain (.+)" line)
                  contents (if (= contents "no other bags.")
                             []
                             (map (fn [s]
                                    (let [[_ count color] (re-find #"(\d+) (.+?) bags?" s)]
                                      [color (Integer. count)]))
                                  (str/split contents #", ")))]
              (assoc acc bag contents)))
          {}
          lines))

(defn can-contain-gold? [rules bag]
  (if (empty? (get rules bag))
    false
    (or (some #(= "shiny gold" (first %)) (get rules bag))
        (some #(can-contain-gold? rules (first %)) (get rules bag)))))

(defn count-outer-bags [rules]
  (count (filter #(can-contain-gold? rules %) (keys rules))))

(defn count-inner-bags [rules bag]
  (let [contents (get rules bag)]
    (if (empty? contents)
      0
      (reduce + (map (fn [[color count]]
                       (+ count (* count (count-inner-bags rules color))))
                     contents)))))

(defn count-bags [rules]
  (count-inner-bags rules "shiny gold"))

(defn -main []
  (let [lines (with-open [rdr (io/reader "input.txt")]
                (doall (line-seq rdr)))
        rules (parse-rules lines)]
    (println "Number of bag colors that can contain at least one shiny gold bag:" (count-outer-bags rules))
    (println "Total number of bags required inside a shiny gold bag:" (count-bags rules))))

(-main)
