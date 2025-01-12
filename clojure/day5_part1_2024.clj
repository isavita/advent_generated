
(defn parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn read-input [filename]
  (let [lines (-> filename slurp (clojure.string/split #"\n"))
        rules (atom [])
        updates (atom [])
        is-update-section (atom false)]
    (doseq [line lines]
      (let [trimmed-line (clojure.string/trim line)]
        (cond
          (empty? trimmed-line) (reset! is-update-section true)
          (not @is-update-section)
          (let [parts (clojure.string/split trimmed-line #"\|")
                x (parse-int (clojure.string/trim (first parts)))
                y (parse-int (clojure.string/trim (second parts)))]
            (when (and x y) (swap! rules conj [x y])))
          :else
          (let [nums (->> (clojure.string/split trimmed-line #",")
                          (map clojure.string/trim)
                          (keep parse-int))]
            (when (seq nums) (swap! updates conj nums))))))
    [@rules @updates]))

(defn is-correctly-ordered? [update rules]
  (let [positions (zipmap update (range))]
    (every?
     (fn [[x y]]
       (let [pos-x (positions x)
             pos-y (positions y)]
         (if (and pos-x pos-y)
           (< pos-x pos-y)
           true)))
     rules)))

(defn solve []
  (let [[rules updates] (read-input "input.txt")]
    (->> updates
         (filter #(is-correctly-ordered? % rules))
         (map #(nth % (quot (count %) 2)))
         (reduce + 0))))

(println (solve))
