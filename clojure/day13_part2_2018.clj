
(defn parse-input [file]
  (let [lines (line-seq (java.io.BufferedReader. (java.io.FileReader. file)))
        carts (atom [])
        tracks (atom [])]
    (doseq [[y line] (map-indexed vector lines)]
      (let [track-line (char-array line)]
        (doseq [[x r] (map-indexed vector line)]
          (case r
            (\> \< \^ \v) (do (swap! carts conj {:x x :y y :direction r :turns 0})
                             (aset track-line x (if (or (= r \>) (= r \<)) \- \|)))
            nil))
        (swap! tracks conj (vec track-line))))
    {:carts (atom @carts) :tracks @tracks}))

(defn move-cart [cart tracks]
  (let [{:keys [x y direction turns]} cart
        new-pos (case direction
                  \> {:x (inc x) :y y}
                  \< {:x (dec x) :y y}
                  \^ {:x x :y (dec y)}
                  \v {:x x :y (inc y)})
        track-char (get-in tracks [(:y new-pos) (:x new-pos)])]
    (cond
      (= track-char \+) (let [new-direction (case (rem turns 3)
                                              0 (case direction
                                                  \> \^
                                                  \< \v
                                                  \^ \<
                                                  \v \>)
                                              2 (case direction
                                                  \> \v
                                                  \< \^
                                                  \^ \>
                                                  \v \<)
                                              direction)]
                           (assoc new-pos :direction new-direction :turns (inc turns)))
      (or (= track-char \/) (= track-char \\)) (let [new-direction (case track-char
                                                                      \/ (case direction
                                                                           \> \^
                                                                           \< \v
                                                                           \^ \>
                                                                           \v \<)
                                                                      \\ (case direction
                                                                           \> \v
                                                                           \< \^
                                                                           \^ \<
                                                                           \v \>))]
                                                   (assoc new-pos :direction new-direction :turns turns))
      :else (assoc new-pos :direction direction :turns turns))))

(defn check-crash [cart carts]
  (first (filter (fn [c] (and (not= c cart) (= (:x c) (:x cart)) (= (:y c) (:y cart)))) carts)))

(defn solve []
  (let [{:keys [carts tracks]} (parse-input "input.txt")]
    (loop []
      (if (<= (count @carts) 1)
        (let [last-cart (first @carts)]
          (println (str (:x last-cart) "," (:y last-cart))))
        (let [sorted-carts (sort-by (juxt :y :x) @carts)
              to-remove (atom #{})]
          (doseq [cart sorted-carts]
            (when (not (@to-remove cart))
              (let [new-cart (move-cart cart tracks)
                    crash-cart (check-crash new-cart @carts)]
                (if crash-cart
                  (do (swap! to-remove conj cart)
                      (swap! to-remove conj crash-cart))
                  (swap! carts (fn [cs] (mapv (fn [c] (if (= c cart) new-cart c)) cs)))))))
          (swap! carts (fn [cs] (vec (remove @to-remove cs))))
          (recur))))))

(solve)
