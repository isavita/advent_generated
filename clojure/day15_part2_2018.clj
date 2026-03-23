
(require '[clojure.string :as str])

(defn get-neighbors [pos]
  (let [[y x] pos] [[(dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]]))

(defn bfs [start walls units]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start 0]) v {start 0}]
    (if-let [[curr d] (peek q)]
      (let [ns (remove #(or (walls %) (units %) (contains? v %)) (get-neighbors curr))]
        (recur (into (pop q) (map #(vector % (inc d)) ns))
               (reduce #(assoc %1 %2 (inc d)) v ns)))
      v)))

(defn move [pos unit units walls]
  (let [enemy-type (if (= (:type unit) \E) \G \E)
        enemies (filter #(= (:type (second %)) enemy-type) units)]
    (if (some (fn [[ep _]] (some #(= ep %) (get-neighbors pos))) enemies)
      pos
      (let [targets (set (for [[ep _] enemies tp (get-neighbors ep) :when (not (or (walls tp) (units tp)))] tp))
            dists (bfs pos walls (dissoc units pos))
            reachable (filter dists targets)]
        (if (empty? reachable)
          pos
          (let [min-dist (apply min (map dists reachable))
                chosen (first (sort (filter #(= (dists %) min-dist) reachable)))
                dists-back (bfs chosen walls (dissoc units pos))]
            (first (sort (filter #(and (not (or (walls %) (units %))) (= (get dists-back % -1) (dec min-dist))) (get-neighbors pos))))))))))

(defn attack [pos unit units]
  (let [enemy-type (if (= (:type unit) \E) \G \E)
        neighs (set (get-neighbors pos))
        targets (filter (fn [[p u]] (and (= (:type u) enemy-type) (neighs p))) units)]
    (when (seq targets)
      (first (sort-by (juxt (comp :hp second) first) targets)))))

(defn run-round [state stop-on-elf-death]
  (let [order (sort (keys (:units state)))]
    (loop [ps order u-map (:units state) acted #{} full-round true]
      (if-let [p (first ps)]
        (if-let [unit (u-map p)]
          (if (acted (:id unit))
            (recur (rest ps) u-map acted full-round)
            (let [enemies (filter #(not= (:type unit) (:type (second %))) u-map)]
              (if (empty? enemies)
                [(assoc state :units u-map) false]
                (let [new-p (move p unit u-map (:walls state))
                      u-map-m (if (= new-p p) u-map (assoc (dissoc u-map p) new-p unit))
                      target (attack new-p unit u-map-m)]
                  (if target
                    (let [[tp tu] target new-hp (- (:hp tu) (:p unit))]
                      (if (<= new-hp 0)
                        (if (and stop-on-elf-death (= (:type tu) \E))
                          [nil false]
                          (recur (rest ps) (dissoc u-map-m tp) (conj acted (:id unit)) full-round))
                        (recur (rest ps) (update-in u-map-m [tp :hp] - (:p unit)) (conj acted (:id unit)) full-round)))
                    (recur (rest ps) u-map-m (conj acted (:id unit)) full-round))))))
          (recur (rest ps) u-map acted full-round))
        [(assoc state :units u-map) true]))))

(defn simulate [init elf-power]
  (let [total-e (count (filter #(= (:type %) \E) (vals (:units init))))
        state (update init :units (fn [us] (into {} (map (fn [[p u]] [p (if (= (:type u) \E) (assoc u :p elf-power) u)]) us))))]
    (loop [curr state rounds 0]
      (let [[nxt full] (run-round curr true)]
        (if (nil? nxt)
          nil
          (let [us (vals (:units nxt))
                gs (filter #(= (:type %) \G) us)]
            (if (empty? gs)
              (* (if full (inc rounds) rounds) (apply + (map :hp us)))
              (if (< (count (filter #(= (:type %) \E) us)) total-e)
                nil
                (recur nxt (inc rounds))))))))))

(defn parse [lines]
  (reduce-kv (fn [s y l]
               (reduce-kv (fn [s x c]
                            (case c
                              \# (update s :walls conj [y x])
                              \G (assoc-in s [:units [y x]] {:type \G :hp 200 :p 3 :id (count (:units s))})
                              \E (assoc-in s [:units [y x]] {:type \E :hp 200 :p 3 :id (count (:units s))})
                              s))
                          s (vec l)))
             {:walls #{} :units {}} (vec lines)))

(let [input (str/split-lines (slurp "input.txt"))
      init (parse input)]
  (loop [p 4]
    (if-let [res (simulate init p)]
      (println res)
      (recur (inc p)))))

