
(require '[clojure.string :as str])

(defn parse-cube-set [s]
  (->> (str/split s #", ")
       (map #(let [[n color] (str/split % #" ")]
               [(keyword color) (Integer/parseInt n)]))
       (into {})))

(defn parse-game [line]
  (let [[game-str sets-str] (str/split line #": ")
        game-id (Integer/parseInt (str/replace game-str #"Game " ""))]
    {:id game-id
     :sets (->> (str/split sets-str #"; ")
                (map parse-cube-set))}))

(defn game-possible? [game max-cubes]
  (every? (fn [set]
            (and (<= (get set :red 0) (get max-cubes :red))
                 (<= (get set :green 0) (get max-cubes :green))
                 (<= (get set :blue 0) (get max-cubes :blue))))
          (:sets game)))

(defn solve [input-file max-cubes]
  (->> (slurp input-file)
       (str/split-lines)
       (map parse-game)
       (filter #(game-possible? % max-cubes))
       (map :id)
       (reduce +)))

(def max-cubes {:red 12 :green 13 :blue 14})

(println (solve "input.txt" max-cubes))
