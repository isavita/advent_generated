
(ns day16.reindeer-maze
  (:require [clojure.java.io :as io]))

(defn read-lines [path]
  (with-open [r (io/reader path)]
    (doall (line-seq r))))

(defn idx [r c cols] (+ c (* r cols)))

(defn char-at [^String s i] (.charAt s i))

(defn solve [lines]
  (let [rows (count lines)
        cols (count (first lines))
        grid (vec (map vec lines))
        ; locate S and E
        s-pos (first (for [r (range rows) c (range cols)
                            :when (= (char-at (nth lines r) c) \S)]
                        [r c]))
        e-pos (first (for [r (range rows) c (range cols)
                            :when (= (char-at (nth lines r) c) \E)]
                        [r c]))
        [sr sc] s-pos
        [er ec] e-pos
        start-id (idx sr sc cols)
        end-id (idx er ec cols)

        ; directions: 0=E,1=S,2=W,3=N
        dr [0 1 0 -1]
        dc [1 0 -1 0]

        ; moves:
        ; forward: (+1) score
        ; rotate: (+1000) score
        ; We'll run Dijkstra on state (pos,dir)
        inf Long/MAX_VALUE

        ; encode state into int
        state-id (fn [pos dir] (+ (* dir (* rows cols)) pos))

        total-states (* 4 rows cols)
        dist (long-array total-states)
        _ (dotimes [i total-states] (aset dist i inf))

        pred-count (int-array total-states) ; not used, but keep room for clarity
        ; predecessors: for each state, store list of predecessor state-ids
        ; We'll store predecessors in a map of vectors (only for states that get settled)
        preds (atom {})]

    (defn push! [pq state d]
      ;; pq is a vector [d state d state ...] as binary min-heap not needed
      ;; We'll implement simple arraylist + sort? too slow.
      ;; Instead: use java.util.PriorityQueue.
      nil)

    (let [^java.util.PriorityQueue pq (java.util.PriorityQueue.
                                        (reify java.util.Comparator
                                          (compare [_ a b]
                                            (Long/compare (first a) (first b)))))]
      (letfn [(offer-state! [pos dir nd]
                (let [sid (state-id pos dir)]
                  (when (< nd (aget dist sid))
                    (aset dist sid nd)
                    (.add pq (vector nd sid)))
                  nil))
              (try-relax [pos dir nd prev-sid]
                (let [sid (state-id pos dir)]
                  (cond
                    (< nd (aget dist sid))
                    (do
                      (aset dist sid nd)
                      (swap! preds assoc sid [])
                      (.add pq (vector nd sid))
                      (swap! preds update sid (fn [_] [prev-sid])))
                    (= nd (aget dist sid))
                    (do
                      (swap! preds update sid (fn [v] (conj (or v []) prev-sid))))
                    :else nil)))]
        ; init: at S facing East (dir=0)
        (let [start-dir 0
              start-state (state-id start-id start-dir)]
          (aset dist start-state 0)
          (.add pq (vector 0 start-state))

          ; Dijkstra
          (while (not (.isEmpty pq))
            (let [[d sid] (.poll pq)]
              (when (= d (aget dist sid))
                (let [pos (mod sid (* rows cols))
                      dir (quot sid (* rows cols))
                      r (quot pos cols)
                      c (mod pos cols)

                      ; forward
                      nd-f (+ d 1)
                      nr (+ r (nth dr dir))
                      nc (+ c (nth dc dir))]
                  ; forward relax
                  (when (and (<= 0 nr (dec rows)) (<= 0 nc (dec cols))
                             (not= (char-at (nth lines nr) nc) \#))
                    (let [npos (idx nr nc cols)
                          nstate (state-id npos dir)]
                      ; try-relax with predecessor sid
                      (try-relax npos dir nd-f sid)))
                  ; rotations
                  (let [dirL (mod (dec dir) 4)
                        dirR (mod (inc dir) 4)]
                    (try-relax pos dirL (+ d 1000) sid)
                    (try-relax pos dirR (+ d 1000) sid)))))))

          ; find best score to reach E (any dir)
          (let [best (reduce min
                             (for [dir (range 4)]
                               (aget dist (state-id end-id dir))))]
            ; collect all states that are on some shortest path to any E-dir:
            ; start from states at E with dist==best and walk backwards using preds.
            (let [on-path (boolean-array total-states)
                  q (java.util.ArrayDeque.)]
              (doseq [dir (range 4)]
                (let [sid (state-id end-id dir)]
                  (when (= (aget dist sid) best)
                    (aset on-path sid true)
                    (.add q sid))))
              (while (not (.isEmpty q))
                (let [sid (.poll q)]
                  (doseq [p (get @preds sid)]
                    (when (not (aget on-path p))
                      (aset on-path p true)
                      (.add q p)))))
              ; count unique tiles (pos) that have any dir on-path
              (let [tile-on (boolean-array (* rows cols))]
                (dotimes [sid total-states]
                  (when (aget on-path sid)
                    (let [pos (mod sid (* rows cols))]
                      (aset tile-on pos true))))
                (count (filter true? (map #(aget tile-on %) (range (* rows cols))))))))))))

(defn -main []
  (let [lines (read-lines "input.txt")
        ans (solve lines)]
    (println ans)))

; entry point
(-main)
