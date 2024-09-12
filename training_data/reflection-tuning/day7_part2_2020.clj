(require '[clojure.string :as str])

(defn parse-bag-rule [rule]
  (let [[bag contents] (str/split rule #" bags contain ")
        inner-bags (when-not (= contents "no other bags.")
                     (for [inner (str/split contents #", ")]
                       (let [[_ count color] (re-find #"(\d+) (.+) bag" inner)]
                         [color (Integer/parseInt count)])))]
    [bag (into {} inner-bags)]))

(defn build-graph [rules]
  (into {} (map parse-bag-rule rules)))

(defn can-contain-shiny-gold? [graph bag]
  (or (contains? (graph bag) "shiny gold")
      (some #(can-contain-shiny-gold? graph %) (keys (graph bag)))))

(defn count-containing-bags [graph]
  (count (filter #(can-contain-shiny-gold? graph %) (keys graph))))

(defn count-bags-inside [graph bag]
  (reduce + (for [[inner-bag count] (graph bag)]
              (+ count (* count (count-bags-inside graph inner-bag))))))

(defn solve-puzzle []
  (let [input (str/split-lines (slurp "input.txt"))
        graph (build-graph input)]
    (println "Part One:" (count-containing-bags graph))
    (println "Part Two:" (count-bags-inside graph "shiny gold"))))

(solve-puzzle)
