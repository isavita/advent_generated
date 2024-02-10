
(defn caught? [depth range]
  (zero? (mod depth (* 2 (dec range)))))

(defn severity [layers]
  (apply + (for [[depth range] layers
                 :when (caught? depth range)]
             (* depth range))))

(defn parse-input []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (->> (line-seq rdr)
         (map #(->> (clojure.string/split % #": ")
                    (map read-string)
                    (vec)))
         (into {}))))

(def input (parse-input))
(println (severity input))
