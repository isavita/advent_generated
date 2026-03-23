(import '[java.util PriorityQueue])

(def costs {\A 1 \B 10 \C 100 \D 1000})
(def entrances [2 4 6 8])
(def room-indices [[11 12] [13 14] [15 16] [17 18]])
(def hallway-stops [0 1 3 5 7 9 10])
(def target-room {\A 0 \B 1 \C 2 \D 3})

(defn get-room-info [pos]
  (when (>= pos 11)
    [(quot (- pos 11) 2) (mod (- pos 11) 2)]))

(defn done? [state pos L]
  (if-let [[ri depth] (get-room-info pos)]
    (and (= ri (target-room L))
         (or (= depth 1)
             (= (get state (second (room-indices ri))) L)))
    false))

(defn path-clear? [state start end]
  (if (= start end) true
    (let [step (if (< start end) 1 -1)]
      (every? #(= (get state %) \.) 
              (range (+ start step) (if (< start end) (inc end) (dec end)) step)))))

(defn get-next-states [state]
  (let [room-moves (for [i (range 19)
                         :let [L (get state i)]
                         :when (not= L \.)
                         :let [ri-t (target-room L)
                               [top bot] (room-indices ri-t)
                               ready? (every? #(or (= % \.) (= % L)) [(get state top) (get state bot)])]
                         :when ready?
                         :let [target (if (= (get state bot) \.) bot top)
                               dist-in (if (= target bot) 2 1)
                               [ri-s d-s] (or (get-room-info i) [nil nil])]
                         :when (if ri-s
                                 (and (not (done? state i L))
                                      (or (zero? d-s) (= \. (get state (first (room-indices ri-s)))))
                                      (= \. (get state (entrances ri-s)))
                                      (path-clear? state (entrances ri-s) (entrances ri-t)))
                                 (path-clear? state i (entrances ri-t)))]
                     (let [dist-out (if ri-s (inc d-s) 0)
                           dist-h (Math/abs (- (if ri-s (entrances ri-s) i) (entrances ri-t)))]
                       [(* (+ dist-out dist-h dist-in) (costs L)) (-> state (assoc i \.) (assoc target L))]))]
    (if-let [m (first room-moves)] [m]
      (for [i (range 11 19)
            :let [L (get state i)]
            :when (not= L \.)
            :let [[ri d] (get-room-info i)
                  e (entrances ri)]
            :when (and (not (done? state i L))
                       (or (zero? d) (= \. (get state (first (room-indices ri)))))
                       (= \. (get state e)))
            h hallway-stops
            :when (path-clear? state e h)]
        [(* (+ d 1 (Math/abs (- e h))) (costs L)) (-> state (assoc i \.) (assoc h L))]))))

(defn parse []
  (let [ls (clojure.string/split-lines (slurp "input.txt"))
        hall (vec (subs (nth ls 1) 1 12))
        r1 (map first (re-seq #"[A-D]" (nth ls 2)))
        r2 (map first (re-seq #"[A-D]" (nth ls 3)))]
    (vec (concat hall (interleave r1 r2)))))

(defn solve []
  (let [init (parse)
        goal (vec "...........AABBCCDD")
        pq (PriorityQueue. 1000 (reify java.util.Comparator (compare [_ a b] (compare (first a) (first b)))))
        _ (.add pq [0 init])
        dist (atom {init 0})]
    (loop []
      (when-let [[d u] (.poll pq)]
        (if (= u goal) (println d)
          (do
            (when (<= d (get @dist u Integer/MAX_VALUE))
              (doseq [[c v] (get-next-states u)]
                (let [nd (+ d c)]
                  (when (< nd (get @dist v Integer/MAX_VALUE))
                    (swap! dist assoc v nd)
                    (.add pq [nd v])))))
            (recur)))))))

(solve)