(ns bingo
  (:require [clojure.string :as str]))

(defn parse-board [board-str]
  (mapv #(mapv read-string (str/split % #"\s+"))
        (str/split-lines board-str)))

(defn parse-input [input]
  (let [[numbers & boards] (str/split input #"\n\n")]
    {:numbers (mapv read-string (str/split numbers #","))
     :boards (mapv parse-board boards)}))

(defn mark-number [board number]
  (mapv (fn [row]
          (mapv #(if (= % number) nil %) row))
        board))

(defn winning-board? [board]
  (or (some #(every? nil? %) board)
      (some #(every? nil? %) (apply map vector board))))

(defn score [board last-number]
  (when (and board last-number)
    (* last-number
       (reduce + (filter identity (flatten board))))))

(defn play-bingo [numbers boards]
  (loop [remaining-numbers numbers
         current-boards boards
         last-number nil]
    (if (or (empty? remaining-numbers) (empty? current-boards))
      nil
      (let [number (first remaining-numbers)
            marked-boards (mapv #(mark-number % number) current-boards)
            winner (first (filter winning-board? marked-boards))]
        (if winner
          {:winner winner :last-number number}
          (recur (rest remaining-numbers) marked-boards number))))))

(defn play-last-bingo [numbers boards]
  (loop [remaining-numbers numbers
         current-boards boards
         last-number nil
         last-winner nil]
    (if (empty? remaining-numbers)
      {:winner last-winner :last-number last-number}
      (let [number (first remaining-numbers)
            marked-boards (mapv #(mark-number % number) current-boards)
            non-winners (remove winning-board? marked-boards)]
        (if (empty? non-winners)
          {:winner (last marked-boards) :last-number number}
          (recur (rest remaining-numbers) non-winners number 
                 (if (= (count non-winners) 1) (first non-winners) last-winner)))))))

(defn solve-part1 [input]
  (let [{:keys [numbers boards]} (parse-input input)
        {:keys [winner last-number]} (play-bingo numbers boards)]
    (score winner last-number)))

(defn solve-part2 [input]
  (let [{:keys [numbers boards]} (parse-input input)
        {:keys [winner last-number]} (play-last-bingo numbers boards)]
    (score winner last-number)))

;; Example usage:
;; (def input (slurp "input.txt"))
;; (println "Part 1:" (solve-part1 input))
;; (println "Part 2:" (solve-part2 input))
