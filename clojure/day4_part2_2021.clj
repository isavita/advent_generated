
(ns day4.core
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[numbers & boards] (str/split input #"\n\n")
        numbers (map #(Integer/parseInt %) (str/split numbers #","))]
    [numbers (map (fn [board]
                    (mapv (fn [row]
                            (mapv #(Integer/parseInt %) (str/split (str/trim row) #"\s+")))
                          (str/split-lines board)))
                  boards)]))

(defn mark-board [board number]
  (mapv (fn [row]
          (mapv (fn [cell]
                  (if (= cell number) nil cell))
                row))
        board))

(defn board-wins? [board]
  (or (some (fn [row] (every? nil? row)) board)  ; Check rows
      (some (fn [col] (every? nil? col)) (apply mapv vector board)) ; Check columns by transposing
      ))

(defn sum-unmarked [board]
  (->> board
       (mapcat (fn [row] (filter identity row)))
       (reduce + 0)))

(defn play-bingo [numbers boards]
  (loop [numbers numbers
         boards boards
         winning-boards []]
    (if (empty? numbers)
      winning-boards ; Return all winning boards along with the last number called
      (let [number (first numbers)
            marked-boards (map #(mark-board % number) boards)
            newly-won (filter board-wins? marked-boards)
            remaining-boards (remove board-wins? marked-boards)]
        (recur (rest numbers)
               remaining-boards
               (if (seq newly-won)
                 (conj winning-boards {:boards newly-won :last-number number})
                 winning-boards))))))

(defn solve-part1 [numbers boards]
   (let [first-win (first (play-bingo numbers boards))]
     (* (:last-number first-win) (sum-unmarked (first (:boards first-win))))))


(defn solve-part2 [numbers boards]
    (let [all-wins (play-bingo numbers boards)
          last-win (last all-wins)]
      (* (:last-number last-win) (sum-unmarked (first (:boards last-win))))))
  

(defn -main []
  (let [input (slurp "input.txt")
        [numbers boards] (parse-input input)]
    (println "Part 1:" (solve-part1 numbers boards))
    (println "Part 2:" (solve-part2 numbers boards))))

(-main)
