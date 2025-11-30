
(require '[clojure.string :as str])

(defn parse-longs [s] (mapv #(Long/parseLong %) (str/split s #",")))

(defn get-mem [mem idx]
  (if (< (count mem) (inc idx))
    (get (into mem (repeat (inc (- idx (count mem))) 0)) idx)
    (get mem idx)))

(defn set-mem [mem idx val]
  (if (< (count mem) (inc idx))
    (into mem (concat (repeat (- idx (count mem)) 0) [val]))
    (assoc mem idx val)))

(defn param [mem rb mode p]
  (case mode
    0 (get-mem mem p)
    1 p
    2 (get-mem mem (+ rb p))))

(defn write-param [mem rb mode p val]
  (case mode
    0 (set-mem mem p val)
    2 (set-mem mem (+ rb p) val)))

(defn run-intcode [program inputs]
  (loop [mem program, ip 0, rb 0, in-idx 0]
    (let [opcode (mod (get-mem mem ip) 100)
          modes [(mod (quot (get-mem mem ip) 100) 10)
                 (mod (quot (get-mem mem ip) 1000) 10)
                 (mod (quot (get-mem mem ip) 10000) 10)]
          p1 (param mem rb (nth modes 0) (get-mem mem (inc ip)))
          p2 (param mem rb (nth modes 1) (get-mem mem (+ ip 2)))
          p3 (get-mem mem (+ ip 3))]
      (case opcode
        99 -1
        1 (recur (write-param mem rb (nth modes 2) p3 (+ p1 p2)) (+ ip 4) rb in-idx)
        2 (recur (write-param mem rb (nth modes 2) p3 (* p1 p2)) (+ ip 4) rb in-idx)
        3 (recur (write-param mem rb (nth modes 0) (get-mem mem (inc ip)) (nth inputs in-idx)) (+ ip 2) rb (inc in-idx))
        4 p1
        5 (recur mem (if (not= p1 0) p2 (+ ip 3)) rb in-idx)
        6 (recur mem (if (= p1 0) p2 (+ ip 3)) rb in-idx)
        7 (recur (write-param mem rb (nth modes 2) p3 (if (< p1 p2) 1 0)) (+ ip 4) rb in-idx)
        8 (recur (write-param mem rb (nth modes 2) p3 (if (= p1 p2) 1 0)) (+ ip 4) rb in-idx)
        9 (recur mem (+ ip 2) (+ rb p1) in-idx)))))

(defn check-beam [program x y]
  (= 1 (run-intcode program [x y])))

(defn -main [& _]
  (let [program (parse-longs (str/trim (slurp "input.txt")))
        grid-size 50]
    (println (count (for [y (range grid-size)
                          x (range grid-size)
                          :when (check-beam program x y)]
                      [x y])))))

(-main)
