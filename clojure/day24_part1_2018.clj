
(require '[clojure.string :as s])

(defn parse-group [id army line]
  (let [units (Long/parseLong (re-find #"\d+" line))
        hp (Long/parseLong (second (re-seq #"\d+" line)))
        initiative (Long/parseLong (last (re-seq #"\d+" line)))
        damage (Long/parseLong (nth (re-seq #"\d+" line) 2))
        type (re-find #"\w+(?= damage)" line)
        parts (re-find #"\((.*)\)" line)
        mods (if parts (s/split (second parts) #"; ") [])
        get-mods (fn [prefix]
                   (if-let [m (first (filter #(s/starts-with? % prefix) mods))]
                     (set (s/split (subs m (count prefix)) #", "))
                     #{}))]
    {:id id :army army :units units :hp hp :damage damage :type type :initiative initiative
     :weak (get-mods "weak to ") :imm (get-mods "immune to ")}))

(defn ep [g] (* (:units g) (:damage g)))

(defn calc-dmg [att dfn]
  (cond ((:imm dfn) (:type att)) 0
        ((:weak dfn) (:type att)) (* 2 (ep att))
        :else (ep att)))

(defn solve []
  (let [input (s/split (s/trim (slurp "input.txt")) #"\n\n")
        parse-army (fn [idx army-str]
                     (let [lines (rest (s/split-lines army-str))]
                       (map-indexed #(parse-group (str idx "-" %1) idx %2) lines)))
        all-groups (into [] (concat (parse-army 1 (first input)) (parse-army 2 (second input))))]
    (loop [groups all-groups]
      (let [active (filter #(> (:units %) 0) groups)
            armies (set (map :army active))]
        (if (or (< (count armies) 2))
          (reduce + (map :units active))
          (let [sorted-for-target (sort-by (juxt (comp - #(ep %)) (comp - :initiative)) active)
                targets (reduce (fn [m att]
                                  (if-let [best (->> active
                                                     (filter #(and (not= (:army %) (:army att))
                                                                  (not (contains? (set (vals m)) (:id %)))))
                                                     (map (fn [dfn] {:dfn dfn :d (calc-dmg att dfn) :ep (ep dfn) :init (:initiative dfn)}))
                                                     (filter #(> (:d %) 0))
                                                     (sort-by (juxt (comp - :d) (comp - :ep) (comp - :init)))
                                                     first)]
                                    (assoc m (:id att) (:id (:dfn best)))
                                    m))
                                {} sorted-for-target)
                attackers (sort-by (comp - :initiative) (filter #(targets (:id %)) active))
                [next-groups total-killed]
                (reduce (fn [[gs killed] att-info]
                          (let [att (first (filter #(= (:id %) (:id att-info)) gs))
                                target-id (targets (:id att))]
                            (if (and att (> (:units att) 0) target-id)
                              (let [dfn (first (filter #(= (:id %) target-id) gs))
                                    dmg (calc-dmg att dfn)
                                    k (min (:units dfn) (quot dmg (:hp dfn)))]
                                [(map #(if (= (:id %) target-id) (update % :units - k) %) gs) (+ killed k)])
                              [gs killed])))
                        [active 0] attackers)]
            (if (zero? total-killed)
              (reduce + (map :units active))
              (recur next-groups))))))))

(println (solve))
