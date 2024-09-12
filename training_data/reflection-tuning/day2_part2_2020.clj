(require '[clojure.string :as str])

(defn parse-line [line]
  (let [[_ min max letter password] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)]
    [(Integer/parseInt min) (Integer/parseInt max) (first letter) password]))

(defn valid-password-part1? [[min max letter password]]
  (let [count (count (filter #(= letter %) password))]
    (<= min count max)))

(defn valid-password-part2? [[pos1 pos2 letter password]]
  (let [char1 (get password (dec pos1))
        char2 (get password (dec pos2))]
    (not= (= char1 letter) (= char2 letter))))

(defn count-valid-passwords [validator]
  (->> (slurp "input.txt")
       (str/split-lines)
       (map parse-line)
       (filter validator)
       (count)))

(let [part1-result (count-valid-passwords valid-password-part1?)
      part2-result (count-valid-passwords valid-password-part2?)]
  (println "Part 1:" part1-result)
  (println "Part 2:" part2-result))
