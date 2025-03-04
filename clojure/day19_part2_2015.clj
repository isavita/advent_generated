
(ns day19
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[rules-str molecule] (str/split input #"\n\n")
        rules (for [line (str/split-lines rules-str)
                    :let [[_ from to] (re-matches #"(\w+) => (\w+)" line)]]
                [from to])]
    [rules molecule]))

(defn apply-rule [molecule [from to]]
  (let [from-len (count from)]
    (loop [i 0
           results #{}]
      (if (< i (count molecule))
        (if (= from (subs molecule i (min (+ i from-len) (count molecule))))
          (recur (inc i) (conj results (str (subs molecule 0 i) to (subs molecule (min (+ i from-len) (count molecule))))))
          (recur (inc i) results))
        results))))


(defn part1 [rules molecule]
  (count (distinct (flatten (for [rule rules]
                              (apply-rule molecule rule))))))

;; Optimized Part 2 (using insights from Reddit and other solutions)
;; The key insight here is realizing the grammar is almost context-free
;; and very simple.  This allows a direct calculation rather than a search.
;; The replacements all follow a very specific pattern:
;; 1. All source molecules (except 'e') only appear on the right-hand side.
;; 2. There are only a few types of rules, and they follow a grammar:
;;   - X => XX   (where X is any element except Rn, Y, Ar)
;;   - X => X Rn X Ar
;;   - X => X Rn X Y X Ar
;;   - X => X Rn X Y X Y X Ar
;;   - e => XX
;;  This means we can simplify the process.

(defn part2 [rules molecule]
   (let [elements (count (re-seq #"[A-Z][a-z]?" molecule))
        rn-ar (count (re-seq #"Rn|Ar" molecule))
        y (count (re-seq #"Y" molecule))]
      (- elements rn-ar (* 2 y) 1)))

(defn -main []
  (let [input (slurp "input.txt")
        [rules molecule] (parse-input input)]
    (println "Part 1:" (part1 rules molecule))
    (println "Part 2:" (part2 rules molecule))))

(-main)
