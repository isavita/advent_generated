
(require '[clojure.string :as s])

(defn solve []
  (let [ls (s/split-lines (s/trim (slurp "input.txt")))
        r1 (map #(nth (nth ls 2) %) [3 5 7 9])
        r4 (map #(nth (nth ls 3) %) [3 5 7 9])
        st (str "..........." (apply str (interleave r1 [\D \C \B \A] [\D \B \A \C] r4)))
        tgt "...........AAAABBBBCCCCDDDD"
        cs {\A 1 \B 10 \C 100 \D 1000}
        rc [2 4 6 8]
        hi [0 1 3 5 7 9 10]
        pq (java.util.PriorityQueue. 10 #(compare (first %1) (first %2)))
        dst (atom {st 0})]
    (.add pq [0 st])
    (loop []
      (if-let [[d g] (.poll pq)]
        (if (= g tgt) d
          (do (when (<= d (get @dst g Long/MAX_VALUE))
                (doseq [h (range 11) :let [c (nth g h)] :when (not= c \.)]
                  (let [ci (- (int c) 65) r (rc ci) rs (+ 11 (* ci 4))
                        step (if (> r h) 1 -1) p (range (+ h step) (+ r step) step)]
                    (when (every? #(= \. (nth g %)) p)
                      (let [sk (mapv #(nth g (+ rs %)) (range 4))]
                        (when (every? #(or (= % \.) (= % c)) sk)
                          (let [ts (last (filter #(= (nth sk %) \.) (range 4)))
                                nc (+ d (* (+ (Math/abs (- r h)) (inc ts)) (cs c)))
                                ng (let [v (char-array g)] (aset v h \.) (aset v (+ rs ts) c) (String. v))]
                            (when (< nc (get @dst ng Long/MAX_VALUE))
                              (swap! dst assoc ng nc) (.add pq [nc ng]))))))))
                (doseq [ri (range 4) :let [rs (+ 11 (* ri 4)) r (rc ri) tc (char (+ 65 ri))]]
                  (if-let [sm (some (fn [s] (let [c (nth g (+ rs s))]
                                             (when (not= c \.)
                                               (when (some #(not= (nth g (+ rs %)) tc) (range s 4)) s)))) (range 4))]
                    (let [c (nth g (+ rs sm))]
                      (doseq [h hi :let [step (if (> h r) 1 -1) p (range r (+ h step) step)]]
                        (when (every? #(= \. (nth g %)) p)
                          (let [nc (+ d (* (+ (Math/abs (- h r)) (inc sm)) (cs c)))
                                ng (let [v (char-array g)] (aset v (+ rs sm) \.) (aset v h c) (String. v))]
                            (when (< nc (get @dst ng Long/MAX_VALUE))
                              (swap! dst assoc ng nc) (.add pq [nc ng])))))))))
              (recur)))
        nil))))

(println (solve))
