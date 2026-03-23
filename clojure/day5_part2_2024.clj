
(require '[clojure.string :as str])

(let [content (slurp "input.txt")
      [rules-part updates-part] (str/split (str/trim content) #"\n\s*\n")
      rules (into #{} (map #(mapv parse-long (str/split % #"\|")) (str/split-lines rules-part)))
      updates (map #(mapv parse-long (str/split % #",")) (str/split-lines updates-part))
      cmp (fn [a b] (cond (rules [a b]) -1 (rules [b a]) 1 :else 0))
      process (fn [u]
                (let [sorted (vec (sort cmp u))]
                  (if (not= u sorted)
                    (nth sorted (quot (count sorted) 2))
                    0)))]
  (println (reduce + (map process updates))))
