
(import '[java.util PriorityQueue HashMap Comparator])

(defn solve []
  (let [lines (try (clojure.string/split-lines (slurp "input.txt"))
                   (catch Exception _ nil))]
    (if (empty? lines)
      nil
      (let [grid-m (into {} (for [y (range (count lines))
                                  x (range (count (first lines)))]
                              [[x y] (nth (nth lines y) x)]))
            [sx sy] (first (for [[k v] grid-m :when (= v \@)] k))
            grid (assoc grid-m [sx sy] \# [sx (inc sy)] \# [sx (dec sy)] \#
                        [(inc sx) sy] \# [(dec sx) sy] \#)
            starts [(vec [(dec sx) (dec sy)]) (vec [(dec sx) (inc sy)])
                    (vec [(inc sx) (dec sy)]) (vec [(inc sx) (inc sy)])]
            all-keys (reduce (fn [m [[_ _] c]]
                               (if (<= (int \a) (int c) (int \z))
                                 (bit-set m (- (int c) (int \a))) m))
                             0 grid)
            pq (PriorityQueue. 10 (reify Comparator
                                    (compare [_ a b] (compare (first a) (first b)))))
            dists (HashMap.)]
        (doseq [i (range 4)]
          (let [st [starts all-keys i]]
            (.put dists st (long 0))
            (.add pq [0 st])))
        (loop []
          (when-not (.isEmpty pq)
            (let [[d [ps mask wid]] (.poll pq)]
              (if (zero? mask)
                (println d)
                (do
                  (let [cached-d (.get dists [ps mask wid])]
                    (when (or (nil? cached-d) (<= (long d) (long cached-d)))
                      (let [[cx cy] (nth ps wid)]
                        (doseq [[dx dy] [[0 1] [0 -1] [1 0] [-1 0]]
                                :let [nx (+ cx dx) ny (+ cy dy)
                                      c (grid [nx ny])]
                                :when (and c (not= c \#))
                                :let [is-door (<= (int \A) (int c) (int \Z))
                                      is-key (<= (int \a) (int c) (int \z))
                                      locked (and is-door (bit-test mask (- (int c) (int \A))))]
                                :when (not locked)
                                :let [new-mask (if (and is-key (bit-test mask (- (int c) (int \a))))
                                                 (bit-clear mask (- (int c) (int \a)))
                                                 mask)
                                      new-ps (assoc ps wid [nx ny])
                                      found-key (not= new-mask mask)
                                      nd (inc d)]]
                          (if found-key
                            (doseq [i (range 4)]
                              (let [ns [new-ps new-mask i]]
                                (when (< nd (long (.getOrDefault dists ns Long/MAX_VALUE)))
                                  (.put dists ns (long nd))
                                  (.add pq [nd ns]))))
                            (let [ns [new-ps new-mask wid]]
                              (when (< nd (long (.getOrDefault dists ns Long/MAX_VALUE)))
                                (.put dists ns (long nd))
                                (.add pq [nd ns]))))))))
                  (recur))))))))))

(solve)
