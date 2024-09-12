(require '[clojure.string :as str])

(defn parse-stacks [stack-lines]
  (let [stack-count (/ (inc (count (last stack-lines))) 4)]
    (vec (for [i (range stack-count)]
           (->> stack-lines
                (map #(nth % (inc (* i 4)) \space))
                (filter #(not= % \space))
                vec)))))

(defn parse-move [move-line]
  (let [[_ count from to] (re-find #"move (\d+) from (\d+) to (\d+)" move-line)]
    [(Integer/parseInt count) (dec (Integer/parseInt from)) (dec (Integer/parseInt to))]))

(defn move-crates [stacks [count from to]]
  (let [moving-crates (take count (stacks from))
        new-from (vec (drop count (stacks from)))
        new-to (vec (concat moving-crates (stacks to)))]
    (-> stacks
        (assoc from new-from)
        (assoc to new-to))))

(defn solve [input]
  (let [[stack-part move-part] (str/split input #"\n\n")
        stack-lines (drop-last (str/split-lines stack-part))
        initial-stacks (parse-stacks stack-lines)
        moves (map parse-move (str/split-lines move-part))
        final-stacks (reduce move-crates initial-stacks moves)]
    (apply str (map first final-stacks))))

(def input (slurp "input.txt"))
(println (solve input))
