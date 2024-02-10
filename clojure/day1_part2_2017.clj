
(defn solve-captcha [input]
  (let [nums (map #(Integer/parseInt (str %)) (seq input))
        len (count nums)
        halfway (/ len 2)]
    (reduce + (map #(if (= %1 %2) %1 0) nums (concat (drop halfway nums) (take halfway nums))))))

(def input (slurp "input.txt"))
(println (solve-captcha input))
