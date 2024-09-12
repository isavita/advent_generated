(require '[clojure.string :as str])

(defn parse-line [line]
  (let [[_ min max letter password] (re-find #"(\d+)-(\d+) (\w): (\w+)" line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :letter (first letter)
     :password password}))

(defn valid-password? [{:keys [min max letter password]}]
  (let [count (count (filter #(= letter %) password))]
    (<= min count max)))

(defn count-valid-passwords [filename]
  (->> (slurp filename)
       str/split-lines
       (map parse-line)
       (filter valid-password?)
       count))

(println (count-valid-passwords "input.txt"))
