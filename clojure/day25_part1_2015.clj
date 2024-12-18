
(defn get-position [row column]
  (let [sum (+ row column)]
    (+ (quot (* (- sum 2) (- sum 1)) 2) column)))

(defn get-code [position]
  (loop [code 20151125
         i 1]
    (if (>= i position)
      code
      (recur (mod (* code 252533) 33554393) (inc i)))))

(let [input (slurp "input.txt")
      [row column] (->> (re-find #"row (\d+), column (\d+)" input)
                        (drop 1)
                        (map #(Integer/parseInt %)))]
  (-> (get-position row column)
      (get-code)
      (println)))
