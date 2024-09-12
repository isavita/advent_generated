(require '[clojure.string :as str])

(defn parse-input [input]
  (let [[instructions & nodes] (str/split-lines input)
        node-map (into {} (map (fn [line]
                                 (let [[node left right] (re-seq #"[A-Z]{3}" line)]
                                   [node [left right]]))
                               nodes))]
    [instructions node-map]))

(defn navigate [instructions node-map]
  (loop [current "AAA"
         steps 0
         [instruction & rest-instructions] (cycle instructions)]
    (if (= current "ZZZ")
      steps
      (let [next-node (get-in node-map [current (if (= instruction \L) 0 1)])]
        (recur next-node (inc steps) rest-instructions)))))

(defn solve []
  (let [input (slurp "input.txt")
        [instructions node-map] (parse-input input)]
    (navigate instructions node-map)))

(println (solve))
