
(let [jets (clojure.string/trim (slurp "input.txt"))
      n-jets (count jets)
      rocks [[[0 0] [1 0] [2 0] [3 0]]
             [[1 0] [0 1] [1 1] [2 1] [1 2]]
             [[0 0] [1 0] [2 0] [2 1] [2 2]]
             [[0 0] [0 1] [0 2] [0 3]]
             [[0 0] [1 0] [0 1] [1 1]]]
      total 1000000000000]
  (loop [r-idx 0
         j-idx 0
         chamber #{}
         tops [0 0 0 0 0 0 0]
         highest 0
         seen {}
         r-count 0
         extra-h 0]
    (if (>= r-count total)
      (println (+ (long highest) (long extra-h)))
      (let [s-idx (int (mod r-idx 5))
            js-idx (int (mod j-idx n-jets))
            profile (mapv #(- (long highest) (long %)) tops)
            state [s-idx js-idx profile]]
        (if (and (seen state) (zero? extra-h))
          (let [[old-rc old-h] (seen state)
                c-len (- r-count old-rc)
                c-h (- highest old-h)
                n-cycles (quot (- total r-count) c-len)]
            (recur r-idx j-idx chamber tops highest seen (+ r-count (* n-cycles c-len)) (* n-cycles c-h)))
          (let [shape (rocks s-idx)
                init-rock (map (fn [[x y]] [(+ x 2) (+ y (long highest) 4)]) shape)
                [final-rock next-j] (loop [curr-rock init-rock curr-j j-idx]
                                      (let [dx (if (= (.charAt jets (int (mod curr-j n-jets))) \>) 1 -1)
                                            pushed (let [m (map (fn [[x y]] [(+ (long x) (long dx)) (long y)]) curr-rock)]
                                                     (if (every? (fn [[x y]] (and (<= 0 x 6) (not (chamber [x y])))) m) m curr-rock))
                                            downed (let [m (map (fn [[x ny]] [(long x) (dec (long ny))]) pushed)]
                                                     (if (every? (fn [[x ny]] (and (> ny 0) (not (chamber [x ny])))) m) m nil))]
                                        (if downed
                                          (recur downed (inc curr-j))
                                          [pushed (inc curr-j)])))
                new-highest (max (long highest) (long (apply max (map second final-rock))))
                new-tops (reduce (fn [t [x y]] (assoc t x (max (long (t x)) (long y)))) tops final-rock)
                new-chamber (let [nc (into chamber final-rock)]
                              (if (zero? (mod r-count 1000))
                                (set (filter (fn [[_ y]] (> y (- new-highest 50))) nc))
                                nc))]
            (recur (inc r-idx) next-j new-chamber new-tops new-highest (assoc seen state [r-count highest]) (inc r-count) extra-h)))))))
