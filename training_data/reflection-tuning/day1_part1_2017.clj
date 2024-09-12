(defn solve-captcha [input]
  (let [digits (map #(Character/digit % 10) input)
        pairs (map vector digits (concat (rest digits) [(first digits)]))]
    (reduce + (map (fn [[a b]] (if (= a b) a 0)) pairs))))

(def input (slurp "input.txt"))
(println (solve-captcha (clojure.string/trim input)))
