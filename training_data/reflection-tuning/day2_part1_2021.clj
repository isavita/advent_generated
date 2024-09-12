(defn parse-command [line]
  (let [[cmd val] (clojure.string/split line #" ")]
    [(keyword cmd) (Integer/parseInt val)]))

(defn process-command [[horizontal depth] [cmd val]]
  (case cmd
    :forward [(+ horizontal val) depth]
    :down [horizontal (+ depth val)]
    :up [horizontal (- depth val)]
    [horizontal depth]))

(defn calculate-position [commands]
  (reduce process-command [0 0] commands))

(defn solve-puzzle []
  (let [commands (map parse-command (line-seq (clojure.java.io/reader "input.txt")))
        [final-horizontal final-depth] (calculate-position commands)]
    (* final-horizontal final-depth)))

(println (solve-puzzle))
