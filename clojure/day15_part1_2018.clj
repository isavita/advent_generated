
(ns solution
  (:require [clojure.string :as str]))

(defn reading-order [[x y]] [y x])

(defn get-dist [start-pos walls occupied]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start-pos 0])
         visited {start-pos 0}]
    (if-let [[curr d] (peek q)]
      (let [neighbors (for [[dx dy] [[0 -1] [-1 0] [1 0] [0 1]]
                            :let [p [(+ (first curr) dx) (+ (second curr) dy)]]
                            :when (and (not (walls p))
                                       (not (occupied p))
                                       (not (contains? visited p)))]
                        p)]
        (recur (into (pop q) (map (fn [p] [p (inc d)]) neighbors))
               (reduce #(assoc %1 %2 (inc d)) visited neighbors)))
      visited)))

(defn move [unit walls units]
  (let [enemies (filter #(not= (:type %) (:type unit)) (vals units))
        enemy-pos (set (map :pos enemies))
        u-pos (:pos unit)
        adj (for [[dx dy] [[0 -1] [-1 0] [1 0] [0 1]]] [(+ (first u-pos) dx) (+ (second u-pos) dy)])]
    (if (some enemy-pos adj)
      unit
      (let [occupied (set (map :pos (vals units)))
            dists (get-dist u-pos walls occupied)
            targets (for [e enemies
                          [dx dy] [[0 -1] [-1 0] [1 0] [0 1]]
                          :let [p [(+ (first (:pos e)) dx) (+ (second (:pos e)) dy)]]
                          :when (and (not (walls p)) (not (occupied p)) (get dists p))]
                      [p (get dists p)])]
        (if (empty? targets)
          unit
          (let [target-pos (first (first (sort-by (fn [[[x y] d]] [d y x]) targets)))
                t-dists (get-dist target-pos walls occupied)
                steps (for [[dx dy] [[0 -1] [-1 0] [1 0] [0 1]]
                            :let [p [(+ (first u-pos) dx) (+ (second u-pos) dy)]]
                            :when (and (not (walls p)) (not (occupied p)) (get t-dists p))]
                        [p (get t-dists p)])
                next-pos (first (first (sort-by (fn [[[x y] d]] [d y x]) steps)))]
            (if next-pos (assoc unit :pos next-pos) unit)))))))

(defn attack [unit units]
  (let [enemy-pos-map (into {} (for [u (vals units) :when (not= (:type u) (:type unit))] [(:pos u) u]))
        u-pos (:pos unit)
        adj-pos (for [[dx dy] [[0 -1] [-1 0] [1 0] [0 1]]] [(+ (first u-pos) dx) (+ (second u-pos) dy)])
        targets (keep enemy-pos-map adj-pos)
        target (first (sort-by (fn [u] [(:hp u) (second (:pos u)) (first (:pos u))]) targets))]
    (if target
      (let [new-hp (- (:hp target) (:atk unit))]
        (if (<= new-hp 0) (dissoc units (:id target)) (assoc-in units [(:id target) :hp] new-hp)))
      units)))

(defn solve []
  (let [input (try (slurp "input.txt") (catch Exception _ ""))
        lines (str/split-lines (str/trim input))]
    (if (seq lines)
      (let [initial (reduce (fn [acc y]
                              (reduce (fn [acc2 x]
                                        (let [c (get-in lines [y x])]
                                          (case c
                                            \# (update acc2 :walls conj [x y])
                                            \E (let [id (count (:units acc2))]
                                                 (update acc2 :units assoc id {:id id :type :E :hp 200 :atk 3 :pos [x y]}))
                                            \G (let [id (count (:units acc2))]
                                                 (update acc2 :units assoc id {:id id :type :G :hp 200 :atk 3 :pos [x y]}))
                                            acc2)))
                                      acc (range (count (first lines)))))
                            {:walls #{} :units {}} (range (count lines)))
            walls (:walls initial)]
        (loop [units (:units initial) rounds 0]
          (let [sorted-ids (map :id (sort-by (comp reading-order :pos) (vals units)))
                [final-units final-rounds complete?]
                (loop [ids sorted-ids curr-units units]
                  (if (empty? ids)
                    [curr-units rounds true]
                    (let [id (first ids) u (get curr-units id)]
                      (if-not u
                        (recur (rest ids) curr-units)
                        (let [enemies (filter #(not= (:type u) (:type %)) (vals curr-units))]
                          (if (empty? enemies)
                            [curr-units rounds false]
                            (let [moved-u (move u walls curr-units)
                                  after-move (assoc curr-units id moved-u)
                                  after-attack (attack moved-u after-move)]
                              (recur (rest ids) after-attack))))))))]
            (if complete?
              (recur final-units (inc rounds))
              (println (* final-rounds (reduce + (map :hp (vals final-units))))))))))))

(solve)
