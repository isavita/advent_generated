
(defn parse-line [line]
  (let [[px py pz vx vy vz] (map #(Double/parseDouble %) (re-seq #"-?\d+\.?\d*" line))]
    {:pos {:x px :y py :z pz} :vel {:x vx :y vy :z vz}}))

(defn parse-input [input]
  (map parse-line input))

(defn intersection-2d [p1 p2]
  (let [det (- (* (:x (:vel p1)) (:y (:vel p2))) (* (:x (:vel p2)) (:y (:vel p1))))]
    (if (zero? det)
      nil
      (let [t1 (/ (- (* (:y (:vel p2)) (- (:x (:pos p2)) (:x (:pos p1)))) (* (:x (:vel p2)) (- (:y (:pos p2)) (:y (:pos p1))))) det)
            t2 (/ (- (* (:y (:vel p1)) (- (:x (:pos p2)) (:x (:pos p1)))) (* (:x (:vel p1)) (- (:y (:pos p2)) (:y (:pos p1))))) det)
            x (+ (:x (:pos p1)) (* (:x (:vel p1)) t1))
            y (+ (:y (:pos p1)) (* (:y (:vel p1)) t1))]
        {:coord {:x x :y y} :t1 t1 :t2 t2}))))

(defn solve [input min-val max-val]
  (let [points (parse-input input)]
    (->> (for [i (range (count points))
               j (range i)]
           (let [p1 (nth points i)
                 p2 (nth points j)
                 intersection (intersection-2d p1 p2)]
             (when intersection
               (let [{:keys [coord t1 t2]} intersection
                     {:keys [x y]} coord]
                 (and (>= x min-val) (<= x max-val)
                      (>= y min-val) (<= y max-val)
                      (>= t1 0) (>= t2 0))))))
         (filter identity)
         count)))

(defn read-file [file-name]
  (-> (slurp file-name)
      (clojure.string/trim)
      (clojure.string/split #"\n")))

(defn -main []
  (let [input (read-file "input.txt")]
    (println (solve input 200000000000000.0 400000000000000.0))))
(-main)
