
(defn parse-int [s] (Integer/parseInt s))

(defn parse-line [line]
  (let [[springs groups] (clojure.string/split line #" ")]
    {:springs springs
     :groups (map parse-int (clojure.string/split groups #","))}))

(defn parse-input [input]
  (map parse-line input))

(defn count-arrangements-recursive [row i-springs i-group i-contiguous-damaged cache]
  (let [{:keys [springs groups]} row]
    (cond
      (= i-springs (count springs))
      (cond
        (and (= i-group (count groups)) (= i-contiguous-damaged 0)) 1
        (and (= i-group (dec (count groups))) (= i-contiguous-damaged (nth groups i-group))) 1
        :else 0)
      (contains? cache [i-springs i-group i-contiguous-damaged]) (get cache [i-springs i-group i-contiguous-damaged])
      :else
      (let [char (nth springs i-springs)
            res (atom 0)]
        (when (or (= char \.) (= char \?))
          (if (= i-contiguous-damaged 0)
            (swap! res + (count-arrangements-recursive row (inc i-springs) i-group i-contiguous-damaged cache))
            (when (= i-contiguous-damaged (nth groups i-group 0))
              (swap! res + (count-arrangements-recursive row (inc i-springs) (inc i-group) 0 cache)))))
        (when (or (= char \#) (= char \?))
          (when (and (< i-group (count groups)) (< i-contiguous-damaged (nth groups i-group)))
            (swap! res + (count-arrangements-recursive row (inc i-springs) i-group (inc i-contiguous-damaged) cache))))
        (let [result @res]
          (assoc! cache [i-springs i-group i-contiguous-damaged] result)
          result)))))

(defn count-arrangements [row]
  (count-arrangements-recursive row 0 0 0 (transient {})))

(defn unfold-row [row unfolding-factor]
  (let [{:keys [springs groups]} row]
    {:springs (clojure.string/join "?" (repeat unfolding-factor springs))
     :groups (flatten (repeat unfolding-factor groups))}))

(defn solve [input]
  (->> (parse-input input)
       (map count-arrangements)
       (reduce +)))

(defn read-file [file-name]
  (clojure.string/split (slurp file-name) #"\n"))

(defn -main []
  (let [input (read-file "input.txt")]
    (println (solve input))))

(-main)
