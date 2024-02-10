(import '[java.io BufferedReader FileReader])

(defn read-lines [filename]
  (with-open [reader (BufferedReader. (FileReader. filename))]
    (loop [lines []]
      (let [line (.readLine reader)]
        (if line
          (recur (conj lines line))
          lines)))))

(defn count-trees [forest right down]
  (let [width (count (first forest))
        height (count forest)]
    (loop [trees 0
           x 0
           y 0]
      (if (< y height)
        (let [current-row (nth forest y)
              current-cell (get current-row (mod x width))]
          (recur (+ trees (if (= current-cell \#) 1 0))
                 (+ x right)
                 (+ y down)))
        trees))))

(def forest (read-lines "input.txt"))
(def trees (count-trees forest 3 1))
(println trees)