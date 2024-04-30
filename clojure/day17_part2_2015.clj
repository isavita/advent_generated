(ns day17
  (:gen-class))

(defn combinations
  "Generate all combinations of a sequence"
  [coll]
  (if (empty? coll)
    '(())
    (let [x (first coll)
          xs (rest coll)
          without-x (combinations xs)
          with-x (map #(conj % x) (combinations xs))]
      (concat with-x without-x))))

(defn solve
  [containers target]
  (let [combs (combinations containers)
        valid-combs (filter #(= target (apply + %)) combs)
        min-containers (apply min (map count valid-combs))
        min-containers-combs (filter #(= min-containers (count %)) valid-combs)]
    (count min-containers-combs)))

(defn -main
  []
  (let [containers (map read-string (clojure.string/split (slurp "input.txt") #"\n"))
        answer (solve containers 150)]
    (println answer)))

(-main)