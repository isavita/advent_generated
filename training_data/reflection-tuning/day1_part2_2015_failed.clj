(ns solution
  (:require [clojure.string :as str]))

(defn process-instructions [instructions]
  (reduce (fn [acc char]
            (case char
              \( (update acc :floor inc)
              \) (update acc :floor dec)
              acc))
          {:floor 0, :basement-pos nil}
          (map-indexed (fn [idx char]
                         (if (= char \))
                           {:char char, :pos (inc idx)}
                           {:char char}))
                       instructions)))

(defn part1 [input]
  (:floor (process-instructions input)))

(defn part2 [input]
  (let [result (reduce (fn [acc {:keys [char pos]}]
                         (let [new-floor (case char
                                           \( (inc (:floor acc))
                                           \) (dec (:floor acc))
                                           (:floor acc))]
                           (if (and (= new-floor -1) (nil? (:basement-pos acc)))
                             (reduced pos)
                             (assoc acc :floor new-floor))))
                       {:floor 0, :basement-pos nil}
                       (map-indexed (fn [idx char] {:char char, :pos (inc idx)}) input))]
    (if (number? result) result nil)))

(defn solve [input]
  (let [instructions (str/trim input)]
    {:part1 (part1 instructions)
     :part2 (part2 instructions)}))

(defn -main [& args]
  (let [input (slurp (or (first args) "input.txt"))]
    (println (solve input))))
