
(require '[clojure.string :as str])

(defn step [state]
  (loop [{:keys [mem ip rb in out] :as s} state]
    (let [ins (get mem ip 0)
          op (mod ins 100)
          m (fn [n] (case n 1 (mod (quot ins 100) 10) 2 (mod (quot ins 1000) 10) 3 (mod (quot ins 10000) 10)))
          gv (fn [n] (let [mode (m n) v (get mem (+ ip n) 0)]
                       (case mode 0 (get mem v 0) 1 v 2 (get mem (+ rb v) 0))))
          sv (fn [n v] (let [mode (m n) addr (get mem (+ ip n) 0)]
                         (case mode 0 (assoc mem addr v) 2 (assoc mem (+ rb addr) v))))]
      (case op
        1 (recur (assoc s :mem (sv 3 (+ (gv 1) (gv 2))) :ip (+ ip 4)))
        2 (recur (assoc s :mem (sv 3 (* (gv 1) (gv 2))) :ip (+ ip 4)))
        3 (if (empty? in)
            (assoc (assoc s :mem (sv 1 -1) :ip (+ ip 2)) :status :waiting)
            (recur (assoc s :mem (sv 1 (first in)) :ip (+ ip 2) :in (subvec in 1))))
        4 (let [v (gv 1) nout (conj out v)]
            (if (= (count nout) 3)
              (assoc s :ip (+ ip 2) :out [] :packet nout :status :packet)
              (recur (assoc s :ip (+ ip 2) :out nout))))
        5 (recur (assoc s :ip (if (not= (gv 1) 0) (gv 2) (+ ip 3))))
        6 (recur (assoc s :ip (if (= (gv 1) 0) (gv 2) (+ ip 3))))
        7 (recur (assoc s :mem (sv 3 (if (< (gv 1) (gv 2)) 1 0)) :ip (+ ip 4)))
        8 (recur (assoc s :mem (sv 3 (if (= (gv 1) (gv 2)) 1 0)) :ip (+ ip 4)))
        9 (recur (assoc s :rb (+ rb (gv 1)) :ip (+ ip 2)))
        99 (assoc s :status :halted)))))

(defn solve []
  (let [prog (-> (slurp "input.txt") str/trim (str/split #",")
                 (->> (map-indexed (fn [i v] [(long i) (Long/parseLong v)])) (into {})))]
    (loop [comps (vec (for [i (range 50)] {:mem prog :ip 0 :rb 0 :in [(long i)] :out []}))]
      (let [res (reduce (fn [acc i]
                          (let [c (step (get-in acc [:comps i]))
                                acc (assoc-in acc [:comps i] (dissoc c :status :packet))]
                            (if (= (:status c) :packet)
                              (let [[dest x y] (:packet c)]
                                (if (= dest 255) (reduced y) (update-in acc [:comps (int dest) :in] conj x y)))
                              acc)))
                        {:comps comps} (range 50))]
        (if (number? res) (println res) (recur (:comps res)))))))

(solve)
