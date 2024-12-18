
(defn parse-input [filename]
  (let [lines (line-seq (java.io.BufferedReader. (java.io.FileReader. filename)))
        parse-line (fn [line]
                     (let [[_ a b] (re-matches #"Step (.) must be finished before step (.) can begin\." line)]
                       [(first a) (first b)]))
        edges (map parse-line lines)
        all-steps (->> edges
                       (reduce (fn [acc [a b]]
                                 (-> acc
                                     (assoc a (or (get acc a) {:id a :duration (+ 61 (- (int a) (int \A)))}))
                                     (assoc b (or (get acc b) {:id b :duration (+ 61 (- (int b) (int \A)))}))))
                               {})
                       (vals)
                       (map (fn [x] [(x :id) x]))
                       (into {}))
        deps (reduce (fn [acc [a b]]
                       (update acc b (fnil conj []) a))
                     {} edges)]
    [deps all-steps]))

(defn simulate-work [deps all-steps num-workers]
  (loop [time 0
         workers (vec (repeat num-workers 0))
         tasks (vec (repeat num-workers nil))
         all-steps all-steps
         deps deps]
    (if (empty? all-steps)
      time
      (let [available (->> all-steps
                           (filter (fn [[step _]] (and (empty? (get deps step)) (not (some #(= step %) tasks)))))
                           (map first)
                           (sort))
            [new-workers new-tasks new-available]
            (loop [i 0
                   workers workers
                   tasks tasks
                   available available]
              (if (or (>= i num-workers) (empty? available))
                [workers tasks available]
                (if (zero? (nth workers i))
                  (recur (inc i)
                         (assoc workers i (:duration (get all-steps (first available))))
                         (assoc tasks i (first available))
                         (rest available))
                  (recur (inc i) workers tasks available))))
            min-duration (->> new-workers (filter pos?) (apply min Integer/MAX_VALUE))
            [updated-workers updated-tasks updated-all-steps updated-deps]
            (loop [i 0
                   workers new-workers
                   tasks new-tasks
                   all-steps all-steps
                   deps deps]
              (if (>= i num-workers)
                [workers tasks all-steps deps]
                (if (pos? (nth workers i))
                  (let [new-duration (- (nth workers i) min-duration)]
                    (if (zero? new-duration)
                      (recur (inc i)
                             (assoc workers i 0)
                             (assoc tasks i nil)
                             (dissoc all-steps (nth tasks i))
                             (reduce (fn [d s] (update d s (fn [v] (remove #(= (nth tasks i) %) v)))) deps (keys all-steps)))
                      (recur (inc i) (assoc workers i new-duration) tasks all-steps deps)))
                  (recur (inc i) workers tasks all-steps deps))))]
        (recur (+ time min-duration)
               updated-workers
               updated-tasks
               updated-all-steps
               updated-deps)))))

(let [[deps all-steps] (parse-input "input.txt")]
  (println (simulate-work deps all-steps 5)))
