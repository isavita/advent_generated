(ns polymer-reduction
  (:require [clojure.java.io :as io]))

(defn react?
  "Check if two characters react (same letter, different case)"
  [a b]
  (and (not= a b)
       (= (clojure.string/lower-case a)
          (clojure.string/lower-case b))))

(defn reduce-polymer
  "Reduce the polymer by reacting adjacent units"
  [polymer]
  (reduce (fn [stack char]
            (if (and (seq stack) (react? (peek stack) char))
              (pop stack)
              (conj stack char)))
          []
          polymer))

(defn solve
  "Solve the polymer reduction problem"
  [input]
  (count (reduce-polymer input)))

(defn -main
  []
  (with-open [rdr (io/reader "input.txt")]
    (let [input (slurp rdr)
          result (solve input)]
      (println result))))

(-main)
