(ns solution
  (:require [clojure.string :as str]))

(defn parse-cubes [s]
  (let [parts (str/split s #", ")]
    (reduce (fn [m part]
              (let [[_ n color] (re-matches #"(\d+) (\w+)" part)]
                (assoc m color (Integer/parseInt n))))
            {} parts)))

(defn parse-game [s]
  (let [[_ id rest] (re-matches #"Game (\d+): (.+)" s)
        sets (str/split rest #"; ")]
    {:id (Integer/parseInt id)
     :sets (map parse-cubes sets)}))

(defn max-cubes [games color]
  (apply max (map #(get % color 0) games)))

(defn power [cubes]
  (reduce * (vals cubes)))

(defn solve [input]
  (let [games (map parse-game (str/split-lines input))]
    (reduce + (map #(power {:red (max-cubes (:sets %) "red")
                            :green (max-cubes (:sets %) "green")
                            :blue (max-cubes (:sets %) "blue")})
                   games))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [input (slurp rdr)]
      (println (solve input)))))

(-main)