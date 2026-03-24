
(require '[clojure.string :as s])

(defn parse-group [id army line]
  (let [re #"(\d+) units each with (\d+) hit points (?:\((.*)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)"
        [_ units hp mods dmg type init] (re-find re line)
        mod-map (->> (s/split (or mods "") #"; ")
                     (map (fn [m] [(cond (s/starts-with? m "weak") :weak (s/starts-with? m "immune") :immune)
                                   (set (s/split (s/replace m #"(weak to |immune to )" "") #", "))]))
                     (into {}))]
    {:id id :army army :units (Long/parseLong units) :hp (Long/parseLong hp)
     :dmg (Long/parseLong dmg) :type type :init (Long/parseLong init)
     :weak (get mod-map :weak #{}) :immune (get mod-map :immune #{})}))

(defn effective-power [{:keys [units dmg]}] (* units dmg))

(defn calc-damage [a d]
  (cond ((:immune d) (:type a)) 0
        ((:weak d) (:type a)) (* 2 (effective-power a))
        :else (effective-power a)))

(defn simulate [groups boost]
  (let [groups (map #(if (= (:army %) :immune) (update % :dmg + boost) %) groups)]
    (loop [gs (into {} (map (juxt :id identity) groups))]
      (let [sorted-ids (sort-by #(let [g (gs %)] [(- (effective-power g)) (- (:init g))]) (keys gs))
            targets (reduce (fn [acc aid]
                              (let [a (gs aid)
                                    opps (remove #(or (= (:army %) (:army a)) (contains? (set (vals acc)) (:id %))) (vals gs))
                                    best (first (sort-by (juxt #(- (calc-damage a %)) #(- (effective-power %)) #(- (:init %))) 
                                                         (filter #(> (calc-damage a %) 0) opps)))]
                                (if best (assoc acc aid (:id best)) acc))) {} sorted-ids)
            attack-order (sort-by #(- (:init (gs %))) (keys gs))
            [next-gs total-killed] (reduce (fn [[curr-gs killed] aid]
                                             (if-let [tid (get targets aid)]
                                               (let [a (curr-gs aid) d (curr-gs tid)]
                                                 (if (and a d (> (:units a) 0))
                                                   (let [k (min (:units d) (quot (calc-damage a d) (:hp d)))]
                                                     [(if (<= (- (:units d) k) 0) (dissoc curr-gs tid)
                                                          (update-in curr-gs [tid :units] - k)) (+ killed k)])
                                                   [curr-gs killed]))
                                               [curr-gs killed])) [gs 0] attack-order)]
        (cond (empty? (filter #(= (:army %) :infection) (vals next-gs))) [:immune (reduce + (map :units (vals next-gs)))]
              (empty? (filter #(= (:army %) :immune) (vals next-gs))) [:infection (reduce + (map :units (vals next-gs)))]
              (zero? total-killed) [:stalemate 0]
              :else (recur next-gs))))))

(let [input (s/split (slurp "input.txt") #"\n\n")
      parse-army (fn [idx name] (map-indexed #(parse-group (+ (* idx 100) %1) name %2) (rest (s/split-lines (input idx)))))
      orig (concat (parse-army 0 :immune) (parse-army 1 :infection))]
  (println (second (simulate orig 0)))
  (println (some (fn [b] (let [[win res] (simulate orig b)] (when (= win :immune) res))) (range 1 10000))))
