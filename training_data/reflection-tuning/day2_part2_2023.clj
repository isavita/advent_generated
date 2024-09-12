(require '[clojure.string :as str])

(defn parse-game [line]
  (let [[game draws] (str/split line #": ")
        game-id (Integer/parseInt (re-find #"\d+" game))
        draws (str/split draws #"; ")
        parse-draw (fn [draw]
                     (reduce (fn [acc part]
                               (let [[n color] (str/split part #" ")]
                                 (assoc acc (keyword color) (max (get acc (keyword color) 0)
                                                                 (Integer/parseInt n)))))
                             {}
                             (str/split draw #", ")))]
    (reduce (fn [acc draw]
              (merge-with max acc (parse-draw draw)))
            {:id game-id}
            draws)))

(defn game-power [game]
  (* (get game :red 1)
     (get game :green 1)
     (get game :blue 1)))

(defn solve-puzzle []
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-game)
       (map game-power)
       (reduce +)))

(println (solve-puzzle))
