
(defn parse-input [input]
  (->> (clojure.string/split-lines input)
       (map #(clojure.string/split % #"-"))
       (reduce (fn [adj [a b]]
                 (-> adj
                     (update a (fnil conj []) b)
                     (update b (fnil conj []) a)))
               {})))

(defn small-cave? [cave]
  (every? #(Character/isLowerCase %) cave))

(defn find-paths [adj allow-double-visit?]
  (letfn [(explore [current-path current-cave visited-small-caves double-visited?]
            (if (= current-cave "end")
              [current-path]
              (->> (get adj current-cave)
                   (remove #(= % "start"))
                   (mapcat (fn [next-cave]
                             (let [small? (small-cave? next-cave)
                                   visited? (contains? visited-small-caves next-cave)
                                   can-visit? (or (not small?)
                                                  (not visited?)
                                                  (and allow-double-visit? (not double-visited?)))]
                               (if can-visit?
                                 (explore (conj current-path next-cave)
                                          next-cave
                                          (if small? (conj visited-small-caves next-cave) visited-small-caves)
                                          (if (and small? visited? (not double-visited?)) true double-visited?))
                                 [])))))))]
    (explore ["start"] "start" #{} false)))

(defn solve [input allow-double-visit?]
  (let [adj (parse-input input)]
    (count (find-paths adj allow-double-visit?))))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve input false))
    (println "Part 2:" (solve input true))))

(-main)
