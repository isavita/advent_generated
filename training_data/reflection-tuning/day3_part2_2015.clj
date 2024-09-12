(defn move [[x y] direction]
  (case direction
    \^ [x (inc y)]
    \v [x (dec y)]
    \> [(inc x) y]
    \< [(dec x) y]
    [x y]))

(defn count-houses [moves alternating?]
  (let [initial-state (if alternating?
                        {:santa [0 0] :robo [0 0] :visited #{[0 0]} :turn :santa}
                        {:pos [0 0] :visited #{[0 0]}})]
    (count (:visited
             (reduce (fn [state dir]
                       (if alternating?
                         (let [mover (:turn state)
                               new-pos (move (mover state) dir)]
                           (-> state
                               (assoc mover new-pos)
                               (update :visited conj new-pos)
                               (update :turn #(if (= % :santa) :robo :santa))))
                         (let [new-pos (move (:pos state) dir)]
                           (-> state
                               (assoc :pos new-pos)
                               (update :visited conj new-pos)))))
                     initial-state
                     moves)))))

(let [input (slurp "input.txt")]
  (println "Part One:" (count-houses input false))
  (println "Part Two:" (count-houses input true)))
