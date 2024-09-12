(ns bingo-solver
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [input]
  (let [[numbers & boards] (str/split input #"\n\n")
        drawn-numbers (map parse-long (str/split numbers #","))
        parsed-boards (map (fn [board]
                             (mapv #(mapv parse-long (str/split % #"\s+"))
                                   (str/split-lines board)))
                           boards)]
    [drawn-numbers parsed-boards]))

(defn board-sets [board]
  (let [rows (map set board)
        cols (map set (apply map vector board))]
    (concat rows cols)))

(defn winner? [board marked]
  (some #(set/subset? % marked) (board-sets board)))

(defn score-board [board marked last-num]
  (let [all-numbers (set (flatten board))
        unmarked (set/difference all-numbers marked)]
    (* (reduce + unmarked) last-num)))

(defn play-bingo [numbers boards]
  (loop [remaining-numbers numbers
         marked #{}
         current-boards boards]
    (if-let [num (first remaining-numbers)]
      (let [new-marked (conj marked num)
            winner (first (filter #(winner? % new-marked) current-boards))]
        (if winner
          (score-board winner new-marked num)
          (recur (rest remaining-numbers) new-marked current-boards)))
      0)))

(defn solve-bingo []
  (let [input (slurp "input.txt")
        [numbers boards] (parse-input input)]
    (play-bingo numbers boards)))

(println (solve-bingo))
