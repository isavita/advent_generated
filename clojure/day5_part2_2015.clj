
(defn nice-string? [s]
  (and
    (re-find #"[aeiou].*[aeiou].*[aeiou]" s)
    (re-find #"(.)\1" s)
    (not (re-find #"(ab|cd|pq|xy)" s))))

(defn nice-string-part-two? [s]
  (and
    (re-find #"(..).*\1" s)
    (re-find #"(.).\1" s)))

(defn count-nice-strings [file nice-fn]
  (->> (slurp file)
       (clojure.string/split-lines)
       (filter nice-fn)
       count))

(println (count-nice-strings "input.txt" nice-string?))
(println (count-nice-strings "input.txt" nice-string-part-two?))
