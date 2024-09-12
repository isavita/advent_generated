(require '[clojure.string :as str])

(defn parse-room [line]
  (let [[_ name sector checksum] (re-find #"([a-z-]+)-(\d+)\[([a-z]+)\]" line)]
    {:name name
     :sector (Integer/parseInt sector)
     :checksum checksum}))

(defn is-real-room? [{:keys [name checksum]}]
  (let [freq (->> (remove #{\-} name)
                  (frequencies)
                  (sort-by (juxt (comp - val) key))
                  (take 5)
                  (map first)
                  (apply str))]
    (= freq checksum)))

(defn decrypt-name [name sector]
  (->> name
       (map #(if (= \- %) \space
                (char (+ (int \a) (mod (+ (- (int %) (int \a)) sector) 26)))))
       (apply str)))

(defn solve-puzzle [input]
  (let [rooms (map parse-room (str/split-lines input))
        real-rooms (filter is-real-room? rooms)]
    (println "Part One:" (reduce + (map :sector real-rooms)))
    (println "Part Two:"
             (:sector
              (first (filter #(str/includes? (decrypt-name (:name %) (:sector %)) "northpole")
                             real-rooms))))))

(solve-puzzle (slurp "input.txt"))
