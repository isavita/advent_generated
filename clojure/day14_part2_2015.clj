
(defn parse-reindeer [line]
  (let [parts (clojure.string/split line #" ")
        speed (Integer/parseInt (nth parts 3))
        fly-time (Integer/parseInt (nth parts 6))
        rest-time (Integer/parseInt (nth parts 13))]
    {:speed speed :fly-time fly-time :rest-time rest-time :distance 0 :points 0 :flying true :time-in-mode 0}))

(defn read-reindeer-details [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (mapv parse-reindeer (line-seq rdr))))

(defn simulate-step [reindeer]
  (let [new-distance (if (:flying reindeer) (+ (:distance reindeer) (:speed reindeer)) (:distance reindeer))
        new-time-in-mode (inc (:time-in-mode reindeer))
        flying? (if (and (:flying reindeer) (= new-time-in-mode (:fly-time reindeer)))
                  false
                  (if (and (not (:flying reindeer)) (= new-time-in-mode (:rest-time reindeer)))
                    true
                    (:flying reindeer)))
        new-time-in-mode (if (not= flying? (:flying reindeer)) 0 new-time-in-mode)]
    (assoc reindeer :distance new-distance :flying flying? :time-in-mode new-time-in-mode)))

(defn simulate-race-with-points [reindeers total-seconds]
  (loop [reindeers reindeers
         seconds total-seconds]
    (if (zero? seconds)
      reindeers
      (let [updated-reindeers (mapv simulate-step reindeers)
            max-distance (apply max (map :distance updated-reindeers))
            reindeers-with-points (mapv #(if (= (:distance %) max-distance) (update % :points inc) %) updated-reindeers)]
        (recur reindeers-with-points (dec seconds))))))

(defn find-max-points [reindeers]
  (apply max (map :points reindeers)))

(let [reindeers (read-reindeer-details "input.txt")
      final-reindeers (simulate-race-with-points reindeers 2503)]
  (println (find-max-points final-reindeers)))
