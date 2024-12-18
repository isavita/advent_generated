
(defn parse-input [line]
  (let [[x-range y-range] (clojure.string/split line #", ")
        [x-min x-max] (map #(Integer/parseInt %) (clojure.string/split (subs x-range 15) #"\.\."))
        [y-min y-max] (map #(Integer/parseInt %) (clojure.string/split (subs y-range 2) #"\.\."))]
    {:x-min x-min :x-max x-max :y-min y-min :y-max y-max}))

(defn is-in-target? [x y {:keys [x-min x-max y-min y-max]}]
  (and (>= x x-min) (<= x x-max) (>= y y-min) (<= y y-max)))

(defn is-moving-away? [x y x-vel y-vel {:keys [x-min x-max y-min y-max]}]
  (or (and (< x x-min) (< x-vel 0))
      (and (> x x-max) (> x-vel 0))
      (and (< y y-min) (< y-vel 0))))

(defn simulate [x-vel y-vel target]
  (loop [x 0 y 0 cur-x-vel x-vel cur-y-vel y-vel max-y 0]
    (cond
      (is-in-target? x y target) max-y
      (is-moving-away? x y cur-x-vel cur-y-vel target) nil
      :else (let [new-x (+ x cur-x-vel)
                  new-y (+ y cur-y-vel)
                  new-max-y (max max-y new-y)
                  new-x-vel (cond
                              (> cur-x-vel 0) (dec cur-x-vel)
                              (< cur-x-vel 0) (inc cur-x-vel)
                              :else 0)
                  new-y-vel (dec cur-y-vel)]
              (recur new-x new-y new-x-vel new-y-vel new-max-y)))))

(defn solve []
  (let [line (first (line-seq (clojure.java.io/reader "input.txt")))
        target (parse-input line)]
    (->> (for [x-vel (range -500 501)
               y-vel (range -500 501)]
           (simulate x-vel y-vel target))
         (remove nil?)
         (apply max))))

(println (solve))
