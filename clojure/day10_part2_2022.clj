
(defn solve []
  (let [lines (clojure.string/split (slurp "input.txt") #"\n")
        x-vals (loop [xs [1] lines lines]
                 (if (empty? lines)
                   xs
                   (let [line (first lines)]
                     (cond
                       (= line "noop") (recur (conj xs (last xs)) (rest lines))
                       (clojure.string/starts-with? line "addx")
                       (let [n (Integer/parseInt (subs line 5))]
                         (recur (conj (conj xs (last xs)) (+ (last xs) n)) (rest lines)))))))
        grid (->> (range (count x-vals))
                  (map (fn [i]
                         (let [crtx (mod i 40)
                               crty (quot i 40)]
                           (if (<= (Math/abs (- crtx (nth x-vals i))) 1)
                             [crtx crty]
                             nil))))
                  (remove nil?)
                  (into #{}))]
    (doseq [y (range 6)]
      (doseq [x (range 40)]
        (print (if (contains? grid [x y]) "#" ".")))
      (println))))

(solve)
