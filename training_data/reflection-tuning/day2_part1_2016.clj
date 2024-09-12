(def keypad
  [[\1 \2 \3]
   [\4 \5 \6]
   [\7 \8 \9]])

(defn move [[x y] direction]
  (case direction
    \U [(max 0 (dec x)) y]
    \D [(min 2 (inc x)) y]
    \L [x (max 0 (dec y))]
    \R [x (min 2 (inc y))]))

(defn process-line [start line]
  (reduce (fn [pos dir] (move pos dir)) start line))

(defn solve-bathroom-code [instructions]
  (loop [lines instructions
         pos [1 1]
         code []]
    (if (empty? lines)
      (apply str code)
      (let [new-pos (process-line pos (first lines))]
        (recur (rest lines)
               new-pos
               (conj code (get-in keypad new-pos)))))))

(defn -main []
  (let [input (slurp "input.txt")
        instructions (clojure.string/split-lines input)]
    (println (solve-bathroom-code instructions))))

(-main)
