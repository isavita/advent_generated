
(ns polymer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [file]
  (let [[template rules] (str/split (slurp file) #"\n\n")
        rules (into {} (for [line (str/split-lines rules)
                             :when (seq line)
                             :let [[pair insert] (str/split line #" -> ")]]
                         [pair insert]))]
    [template rules]))

(defn step [rules pairs]
  (reduce-kv
   (fn [acc pair cnt]
     (if-let [mid (rules pair)]
       (let [[a b] (seq pair)]
         (-> acc
             (update (str a mid) (fnil + 0) cnt)
             (update (str mid b) (fnil + 0) cnt)))
       (update acc pair (fnil + 0) cnt)))
   {} pairs))

(defn solve [template rules]
  (let [pairs (frequencies (map str template (rest template)))
        final-pairs (nth (iterate #(step rules %) pairs) 40)
        counts (reduce-kv
                (fn [acc pair cnt]
                  (update acc (first pair) (fnil + 0) cnt))
                {(last template) 1}
                final-pairs)
        freqs (vals counts)]
    (- (apply max freqs) (apply min freqs))))

(let [[template rules] (read-input "input.txt")]
  (println (solve template rules)))
