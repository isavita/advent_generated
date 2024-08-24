(ns day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [filename]
  (let [lines (line-seq (io/reader filename))
        draws (mapv #(Integer/parseInt %) (str/split (first lines) #","))
        boards (->> (rest lines)
                    (remove str/blank?)
                    (partition 5)
                    (mapv (fn [board]
                            (mapv (fn [line]
                                    (mapv #(Integer/parseInt %) (str/split (str/trim line) #"\s+")))
                                  board))))]
    [draws boards]))

(defn mark-number [board number]
  (mapv (fn [row]
          (mapv (fn [n] (if (= n number) nil n)) row))
        board))

(defn check-winner [board]
  (let [rows (some #(every? nil? %) board)
        cols (some #(every? nil? %) (apply mapv vector board))]
    (or rows cols)))

(defn calculate-score [board last-number]
  (* last-number (reduce + (mapv (fn [row] (reduce + (filter some? row))) board))))

(defn play-bingo [draws boards]
  (loop [draws draws
         boards boards]
    (let [number (first draws)
          marked-boards (mapv #(mark-number % number) boards)
          winner (some #(when (check-winner %) %) marked-boards)]
      (if winner
        (calculate-score winner number)
        (recur (rest draws) marked-boards)))))

(defn -main []
  (let [[draws boards] (parse-input "input.txt")]
    (println (play-bingo draws boards))))

(-main)