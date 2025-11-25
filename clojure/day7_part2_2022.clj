
(require '[clojure.string :as str])

(defn main []
  (let [root [""]
        dirs (atom {"" 0})
        files (atom {})
        curr (atom root)]
    (with-open [rdr (clojure.java.io/reader "input.txt")]
      (doseq [line (line-seq rdr)]
        (let [txt (str/split line #"\s+")]
          (if (= (first txt) "$")
            (if (= (second txt) "cd")
              (case (nth txt 2)
                "/"  (reset! curr (vec root))
                ".." (swap! curr pop)
                (swap! curr conj (nth txt 2)))
              (swap! dirs assoc (str/join "/" @curr) 0))
            (when (re-matches #"\d+" (first txt))
              (swap! files assoc (str/join "/" (conj @curr (second txt)))
                     (Long/parseUnsignedLong (first txt))))))))
    (doseq [[f s] @files]
      (let [path (str/split f #"/")]
        (doseq [i (range 1 (count path))]
          (let [k (str/join "/" (take i path))]
            (swap! dirs update k (fnil + 0) s)))))
    (let [sorted (sort (vals @dirs))
          total 70000000
          want 30000000
          available (- total (get @dirs ""))]
      (println (first (filter #(>= % (- want available)) sorted))))))

(main)
