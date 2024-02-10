
(require '[clojure.string :as str])

(defn count-orbits [orbits]
  (let [orbit-map (reduce (fn [acc orbit]
                            (assoc acc (second (str/split orbit #"\)")) (first (str/split orbit #"\)"))))
                          {} orbits)]
    (defn count-orbits-helper [obj]
      (if-let [direct-orbit (get orbit-map obj)]
        (inc (count-orbits-helper direct-orbit))
        0))
    (apply + (map count-orbits-helper (keys orbit-map)))))

(def input (slurp "input.txt"))
(println (count-orbits (str/split-lines input)))
