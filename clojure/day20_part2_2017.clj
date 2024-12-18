
(defn parse-coords [s]
  (map #(Integer/parseInt %) (clojure.string/split (subs s 3 (dec (count s))) #",")))

(defn parse-particle [line]
  (let [[p v a] (clojure.string/split line #", ")]
    {:p (parse-coords p) :v (parse-coords v) :a (parse-coords a)}))

(defn update-particle [particle]
  (let [{:keys [p v a]} particle
        new-v (map + v a)
        new-p (map + p new-v)]
    {:p new-p :v new-v :a a}))

(defn pos-str [particle]
  (clojure.string/join "," (:p particle)))

(defn solve []
  (let [particles (->> (slurp "input.txt")
                       (clojure.string/split-lines)
                       (map parse-particle)
                       (vec))]
    (loop [particles particles
           tick 0]
      (if (>= tick 1000)
        (count particles)
        (let [updated-particles (map update-particle particles)
              positions (frequencies (map pos-str updated-particles))
              new-particles (filter (fn [p] (= 1 (get positions (pos-str p)))) updated-particles)]
          (recur new-particles (inc tick)))))))

(println (solve))
