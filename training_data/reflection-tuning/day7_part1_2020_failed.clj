(require '[clojure.string :as str])

(defn parse-bag-rule [rule]
  (let [[color contents] (str/split rule #" bags contain ")
        content-list (re-seq #"(\d+) ([^,]+) bag" contents)]
    [color (into {} (map (fn [[_ num color]] [color (Integer/parseInt num)]) content-list))]))

(defn parse-rules [input]
  (into {} (map parse-bag-rule (str/split-lines input))))

(defn can-contain-shiny-gold? [rules color]
  (if (= color "shiny gold")
    false
    (or (contains? (get rules color) "shiny gold")
        (some #(can-contain-shiny-gold? rules %) (keys (get rules color))))))

(defn count-bags-containing-shiny-gold [input]
  (let [rules (parse-rules input)]
    (->> (keys rules)
         (filter #(can-contain-shiny-gold? rules %))
         count)))

;; Example usage:
(def sample-input
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(println (count-bags-containing-shiny-gold sample-input))
