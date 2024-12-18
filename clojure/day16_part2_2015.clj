
(ns aunt-sue
  (:require [clojure.string :as str]))

(def mfcsam-reading
  {:children 3, :cats 7, :samoyeds 2, :pomeranians 3, :akitas 0,
   :vizslas 0, :goldfish 5, :trees 3, :cars 2, :perfumes 1})

(defn parse-aunt-sue [line]
  (let [[_ id & props] (str/split line #"[: ,]+")
        prop-pairs (partition 2 props)]
    {:id (Integer/parseInt id)
     :props (into {} (map (fn [[k v]] [(keyword k) (Integer/parseInt v)]) prop-pairs))}))

(defn matches-part1? [aunt]
  (every? (fn [[k v]] (or (not (contains? (:props aunt) k)) (= v (get (:props aunt) k))))
          mfcsam-reading))

(defn matches-part2? [aunt]
  (every? (fn [[k v]]
            (let [aunt-val (get (:props aunt) k)]
              (or (not aunt-val)
                  (case k
                    :cats (> aunt-val v)
                    :trees (> aunt-val v)
                    :pomeranians (< aunt-val v)
                    :goldfish (< aunt-val v)
                    (= aunt-val v)))))
          mfcsam-reading))

(defn solve [input-file match-fn]
  (->> (slurp input-file)
       (str/split-lines)
       (map parse-aunt-sue)
       (filter match-fn)
       (first)
       (:id)))

(defn -main []
  (println "Part 1:" (solve "input.txt" matches-part1?))
  (println "Part 2:" (solve "input.txt" matches-part2?)))

(-main)
