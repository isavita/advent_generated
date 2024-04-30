(defn parse-instruction [line]
  (let [[op arg] (clojure.string/split line #"\s+")
        arg (Integer/parseInt arg)]
    {:op (keyword op) :arg arg}))

(defn run-program [instructions]
  (loop [acc 0 ip 0 visited #{}]
    (if (contains? visited ip)
      acc
      (let [{:keys [op arg]} (nth instructions ip)
            new-ip (inc ip)]
        (case op
          :acc (recur (+ acc arg) new-ip (conj visited ip))
          :jmp (recur acc (+ ip arg) (conj visited ip))
          :nop (recur acc new-ip (conj visited ip)))))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [instructions (map parse-instruction (line-seq rdr))]
      (println (run-program instructions)))))

(-main)