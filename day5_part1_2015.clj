
(defn nice-string? [s]
  (and
    (re-find #"[aeiou].*[aeiou].*[aeiou]" s)
    (re-find #"(.)\1" s)
    (not (re-find #"(ab|cd|pq|xy)" s))))

(def input (slurp "input.txt"))
(def nice-strings (count (filter nice-string? (clojure.string/split-lines input))))
(println nice-strings)
