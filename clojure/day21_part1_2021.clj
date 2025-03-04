
(ns day21
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(->> (re-find #"Player \d starting position: (\d+)" %)
                   second
                   Integer/parseInt))
       (vec)))

(defn move [position steps]
  (inc (mod (+ position steps -1) 10)))

(defn play-game [starting-positions]
  (loop [positions starting-positions
         scores [0 0]
         player 0
         die-rolls 0
         die-val 1]
    (let [roll1 die-val
          roll2 (inc (mod die-val 100))
          roll3 (inc (mod (inc die-val) 100))
          total-roll (+ roll1 roll2 roll3)
          new-position (move (nth positions player) total-roll)
          new-score (+ (nth scores player) new-position)
          new-scores (assoc scores player new-score)
          new-positions (assoc positions player new-position)
          new-die-rolls (+ die-rolls 3)
          new-die-val (inc (mod (+ die-val 2) 100))]
      (if (>= new-score 1000)
        {:winner player :loser (if (= player 0) 1 0) :scores new-scores :die-rolls new-die-rolls}
        (recur new-positions new-scores (if (= player 0) 1 0) new-die-rolls new-die-val)))))


(defn -main []
  (let [input (slurp "input.txt")
        starting-positions (parse-input input)
        result (play-game starting-positions)
        loser-score (nth (:scores result) (:loser result))]
      (println (* loser-score (:die-rolls result)))))

(-main)
