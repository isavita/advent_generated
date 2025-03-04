
(defn parse-value [registers x]
  (if (re-matches #"[a-z]" x)
    (get registers x 0)
    (Integer/parseInt x)))

(defn execute [instructions registers pc last-sound]
  (if (or (< pc 0) (>= pc (count instructions)))
    {:recovered last-sound :registers registers :pc pc :last-sound last-sound}
    (let [[instruction x y] (nth instructions pc)
          val-x (parse-value registers x)
          val-y (if y (parse-value registers y) nil)]
      (case instruction
        "snd" {:registers registers :pc (inc pc) :last-sound val-x}
        "set" {:registers (assoc registers x val-y) :pc (inc pc) :last-sound last-sound}
        "add" {:registers (assoc registers x (+ val-x val-y)) :pc (inc pc) :last-sound last-sound}
        "mul" {:registers (assoc registers x (* val-x val-y)) :pc (inc pc) :last-sound last-sound}
        "mod" {:registers (assoc registers x (mod val-x val-y)) :pc (inc pc) :last-sound last-sound}
        "rcv" (if (not= val-x 0)
                {:recovered last-sound :registers registers :pc pc :last-sound last-sound}
                {:registers registers :pc (inc pc) :last-sound last-sound})
        "jgz" (if (> val-x 0)
                {:registers registers :pc (+ pc val-y) :last-sound last-sound}
                {:registers registers :pc (inc pc) :last-sound last-sound})))))

(defn solve [instructions]
  (loop [registers {} pc 0 last-sound nil]
    (let [result (execute instructions registers pc last-sound)]
      (if (:recovered result)
        (:recovered result)
        (recur (:registers result) (:pc result) (:last-sound result))))))

(defn parse-instruction [line]
  (let [parts (clojure.string/split line #" ")]
    (if (= (count parts) 2)
      [(first parts) (second parts) nil]
      [(first parts) (second parts) (nth parts 2)])))

(defn main []
  (try
    (let [lines (with-open [rdr (clojure.java.io/reader "input.txt")]
                      (doall (line-seq rdr)))
          instructions (map parse-instruction lines)
          result (solve instructions)]
      (println result))
    (catch Exception e
      (println "Error:" (.getMessage e)))))

(main)
