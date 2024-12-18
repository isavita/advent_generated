
(defn solve []
  (let [input (slurp "input.txt")
        initial-cups (map #(Integer/parseInt (str %)) input)
        total-cups 1000000
        total-moves 10000000
        cups (int-array (inc total-cups))
        last-cup (atom nil)]
    (loop [i 0]
      (when (< i (count initial-cups))
        (let [cup (nth initial-cups i)]
          (when @last-cup
            (aset cups @last-cup cup))
          (reset! last-cup cup)
          (recur (inc i)))))
    (loop [i (inc (count initial-cups))]
      (when (<= i total-cups)
        (aset cups @last-cup i)
        (reset! last-cup i)
        (recur (inc i))))
    (aset cups @last-cup (first initial-cups))
    (loop [current-cup (first initial-cups)
           move 0]
      (when (< move total-moves)
        (let [pickup1 (aget cups current-cup)
              pickup2 (aget cups pickup1)
              pickup3 (aget cups pickup2)
              destination-cup (loop [dc (dec current-cup)]
                                (if (zero? dc)
                                  total-cups
                                  (if (or (= dc pickup1) (= dc pickup2) (= dc pickup3))
                                    (recur (dec dc))
                                    dc)))]
          (aset cups current-cup (aget cups pickup3))
          (aset cups pickup3 (aget cups destination-cup))
          (aset cups destination-cup pickup1)
          (recur (aget cups current-cup) (inc move)))))
    (let [cup1 (aget cups 1)
          cup2 (aget cups cup1)]
      (* cup1 cup2))))

(println (solve))
