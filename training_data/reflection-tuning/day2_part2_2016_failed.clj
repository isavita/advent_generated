(ns bathroom-security
  (:require [clojure.string :as str]))

(def keypad-1
  {'1 {'D '4, 'R '2},       '2 {'L '1, 'D '5, 'R '3}, '3 {'L '2, 'D '6},
   '4 {'U '1, 'D '7, 'R '5}, '5 {'U '2, 'L '4, 'D '8, 'R '6}, '6 {'U '3, 'L '5, 'D '9},
   '7 {'U '4, 'R '8},       '8 {'U '5, 'L '7, 'R '9}, '9 {'U '6, 'L '8}})

(def keypad-2
  {'1 {'D '3},
   '2 {'R '3, 'D '6},       '3 {'U '1, 'L '2, 'R '4, 'D '7}, '4 {'L '3, 'D '8},
   '5 {'R '6},              '6 {'U '2, 'L '5, 'R '7, 'D 'A}, '7 {'U '3, 'L '6, 'R '8, 'D 'B},
   '8 {'U '4, 'L '7, 'R '9, 'D 'C}, '9 {'L '8},
   'A {'U '6, 'R 'B},       'B {'U '7, 'L 'A, 'R 'C, 'D 'D}, 'C {'U '8, 'L 'B},
   'D {'U 'B}})

(defn move [keypad pos instruction]
  (get-in keypad [pos instruction] pos))

(defn process-instructions [keypad start instructions]
  (reduce (fn [acc line]
            (let [pos (last acc)]
              (conj acc (reduce #(move keypad %1 %2) pos line))))
          [start]
          instructions))

(defn solve [keypad start instructions]
  (apply str (rest (process-instructions keypad start instructions))))

(let [instructions (str/split-lines (slurp "input.txt"))]
  (println "Part 1:" (solve keypad-1 '5 instructions))
  (println "Part 2:" (solve keypad-2 '5 instructions)))
