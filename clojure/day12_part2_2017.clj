
(defn dfs [node adj visited]
  (if (contains? visited node)
    visited
    (let [visited (conj visited node)]
      (reduce (fn [v n] (dfs n adj v)) visited (get adj node [])))))

(defn solve []
  (let [lines (-> "input.txt" slurp (clojure.string/split #"\n"))
        adj (reduce
              (fn [m line]
                (let [[from to-str] (clojure.string/split line #" <-> ")
                      from (Integer/parseInt from)
                      tos (map #(Integer/parseInt %) (clojure.string/split to-str #", "))]
                  (reduce (fn [m to]
                            (-> m
                                (update from #(if % (conj % to) [to]))
                                (update to #(if % (conj % from) [from]))))
                          m tos)))
              {} lines)
        nodes (keys adj)]
    (loop [visited #{} groups 0 remaining nodes]
      (if (empty? remaining)
        groups
        (let [node (first remaining)
              new-visited (dfs node adj visited)
              new-remaining (remove new-visited remaining)]
          (recur new-visited (inc groups) new-remaining))))))

(println (solve))
