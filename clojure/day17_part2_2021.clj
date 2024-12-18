
(defn parse-input [line]
  (let [[x-range y-range] (clojure.string/split line #", ")
        [x-min x-max] (map #(Integer/parseInt %) (clojure.string/split (subs x-range 15) #"\.\."))
        [y-min y-max] (map #(Integer/parseInt %) (clojure.string/split (subs y-range 2) #"\.\."))]
    {:x-min x-min :x-max x-max :y-min y-min :y-max y-max}))

(defn is-moving-away? [x-pos y-pos x-vel y-vel {:keys [x-min x-max y-min y-max]}]
  (or (and (< x-pos x-min) (< x-vel 0))
      (and (> x-pos x-max) (> x-vel 0))
      (and (< y-pos y-min) (< y-vel 0))))

(defn simulate [x-vel y-vel {:keys [x-min x-max y-min y-max]}]
  (loop [x-pos 0 y-pos 0 cur-x-vel x-vel cur-y-vel y-vel]
    (cond
      (and (>= x-pos x-min) (<= x-pos x-max) (>= y-pos y-min) (<= y-pos y-max)) true
      (is-moving-away? x-pos y-pos cur-x-vel cur-y-vel {:x-min x-min :x-max x-max :y-min y-min :y-max y-max}) false
      :else (recur
             (+ x-pos cur-x-vel)
             (+ y-pos cur-y-vel)
             (cond (> cur-x-vel 0) (dec cur-x-vel) (< cur-x-vel 0) (inc cur-x-vel) :else cur-x-vel)
             (dec cur-y-vel)))))

(defn solve []
  (let [input (slurp "input.txt")
        target (parse-input input)]
    (count (filter identity (for [x-vel (range -1000 1001)
                                  y-vel (range -1000 1001)]
                              (simulate x-vel y-vel target))))))

(println (solve))
