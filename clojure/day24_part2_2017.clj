
(ns day24
  (:require [clojure.string :as str]))

(defn parse-component [line]
  (mapv parse-long (str/split line #"/")))

(defn parse-input [input]
  (mapv parse-component (str/split-lines input)))

(defn build-bridges [components start]
  (letfn [(find-bridges [current-bridge unused-components current-port]
            (let [possible-next (filter #(or (= (first %) current-port)
                                             (= (second %) current-port))
                                       unused-components)]
              (if (empty? possible-next)
                [current-bridge]
                (mapcat (fn [next-comp]
                          (let [next-port (if (= (first next-comp) current-port)
                                            (second next-comp)
                                            (first next-comp))
                                next-unused (remove #(= % next-comp) unused-components)
                                updated-bridge (conj current-bridge next-comp)]
                            (find-bridges updated-bridge next-unused next-port)))
                        possible-next))))]
    (find-bridges [] components start)))

(defn bridge-strength [bridge]
  (reduce + (mapcat identity bridge)))


(defn solve-part1 [components]
  (let [bridges (build-bridges components 0)]
    (apply max (map bridge-strength bridges))))

(defn solve-part2 [components]
    (let [bridges (build-bridges components 0)
          max-length (apply max (map count bridges))]
      (->> bridges
        (filter #(= (count %) max-length))
        (map bridge-strength)
        (apply max))))
  

(defn -main []
  (let [input (slurp "input.txt")
        components (parse-input input)]
    (println "Part 1:" (solve-part1 components))
    (println "Part 2:" (solve-part2 components))))

(-main)
