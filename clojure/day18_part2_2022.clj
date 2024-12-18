
(defn parse-pt3 [s]
  (let [[x y z] (map #(Integer/parseInt %) (clojure.string/split s #","))]
    {:x x :y y :z z}))

(defn add-pt3 [p1 p2]
  {:x (+ (:x p1) (:x p2))
   :y (+ (:y p1) (:y p2))
   :z (+ (:z p1) (:z p2))})

(defn min-pt3 [p1 p2]
  {:x (min (:x p1) (:x p2))
   :y (min (:y p1) (:y p2))
   :z (min (:z p1) (:z p2))})

(defn max-pt3 [p1 p2]
  {:x (max (:x p1) (:x p2))
   :y (max (:y p1) (:y p2))
   :z (max (:z p1) (:z p2))})

(defn solve []
  (let [lines (-> "input.txt" slurp clojure.string/split-lines)
        cubes (->> lines
                   (remove clojure.string/blank?)
                   (map parse-pt3)
                   (reduce (fn [m p] (assoc m p true)) {}))
        min-pt (reduce min-pt3 {:x Integer/MAX_VALUE :y Integer/MAX_VALUE :z Integer/MAX_VALUE} (keys cubes))
        max-pt (reduce max-pt3 {:x Integer/MIN_VALUE :y Integer/MIN_VALUE :z Integer/MIN_VALUE} (keys cubes))
        min-pt (add-pt3 min-pt {:x -1 :y -1 :z -1})
        max-pt (add-pt3 max-pt {:x 1 :y 1 :z 1})
        neighbors [{:x -1 :y 0 :z 0} {:x 1 :y 0 :z 0} {:x 0 :y -1 :z 0} {:x 0 :y 1 :z 0} {:x 0 :y 0 :z -1} {:x 0 :y 0 :z 1}]
        q (java.util.LinkedList. (list min-pt))
        seen (atom #{min-pt})
        faces (atom 0)]
    (while (not (.isEmpty q))
      (let [curr (.poll q)]
        (doseq [delta neighbors]
          (let [next (add-pt3 curr delta)]
            (when (and (>= (:x next) (:x min-pt))
                       (>= (:y next) (:y min-pt))
                       (>= (:z next) (:z min-pt))
                       (<= (:x next) (:x max-pt))
                       (<= (:y next) (:y max-pt))
                       (<= (:z next) (:z max-pt)))
              (if (contains? cubes next)
                (swap! faces inc)
                (when (not (contains? @seen next))
                  (swap! seen conj next)
                  (.add q next))))))))
    (println @faces)))

(solve)
