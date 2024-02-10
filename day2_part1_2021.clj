
(defn process-command [command]
  (let [[action value] (clojure.string/split command #" ")
        value (Integer/parseInt value)]
    (cond
      (= action "forward") [value 0]
      (= action "down") [0 value]
      (= action "up") [0 (- value)])))

(defn main []
  (let [commands (slurp "input.txt")
        [horizontal depth] (reduce (fn [[h d] command]
                                     (let [[dh dd] (process-command command)]
                                       [(+ h dh) (+ d dd)]))
                                   [0 0]
                                   (clojure.string/split commands #"\n"))]
    (* horizontal depth)))

(println (main))
