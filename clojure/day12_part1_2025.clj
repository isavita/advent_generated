
(ns solution
  (:require [clojure.string :as str]))

(defn get-orientations [pts]
  (let [trans-fn (fn [i [x y]]
                   (case i
                     0 [x y] 1 [y (- x)] 2 [(- x) (- y)] 3 [(- y) x]
                     4 [(- x) y] 5 [y x] 6 [x (- y)] 7 [(- y) (- x)]))
        normalize (fn [pts]
                    (let [mx (apply min (map first pts))
                          my (apply min (map second pts))]
                      (vec (sort (map (fn [[x y]] [(- x mx) (- y my)]) pts)))))]
    (->> (range 8)
         (map (fn [i] (normalize (map #(trans-fn i %) pts))))
         distinct
         (mapv (fn [pts]
                 (let [mx (if (seq pts) (apply max (map first pts)) -1)
                       my (if (seq pts) (apply max (map second pts)) -1)]
                   {:pts pts :w (inc mx) :h (inc my)}))))))

(defn solve [idx ^booleans grid w h shapes rem-area free-area]
  (if (== idx (count shapes))
    true
    (if (> rem-area free-area)
      false
      (let [shape (shapes idx)
            area (:area shape)]
        (loop [ors (:orientations shape)]
          (if-let [orient (first ors)]
            (let [ow (:w orient) oh (:h orient) ^ints pts-off (:off orient)]
              (if (and (<= ow w) (<= oh h))
                (if (loop [r 0]
                      (if (<= r (- h oh))
                        (let [row-off (* r w)]
                          (if (loop [c 0]
                                (if (<= c (- w ow))
                                  (let [pos (+ row-off c)]
                                    (if (loop [pi (int 0)]
                                          (if (< pi (alength pts-off))
                                            (if (aget grid (+ pos (aget pts-off pi))) false (recur (inc pi)))
                                            true))
                                      (do (dotimes [pi (alength pts-off)] (aset grid (+ pos (aget pts-off pi)) true))
                                          (if (solve (inc idx) grid w h shapes (- rem-area area) (- free-area area))
                                            true
                                            (do (dotimes [pi (alength pts-off)] (aset grid (+ pos (aget pts-off pi)) false))
                                                (recur (inc c)))))
                                      (recur (inc c))))
                                  false))
                            true
                            (recur (inc r))))
                        false))
                  true
                  (recur (next ors)))
                (recur (next ors))))
            false))))))

(defn -main []
  (let [content (try (slurp "input.txt") (catch Exception _ ""))
        lines (str/split-lines content)
        shapes-ref (atom [])
        total (atom 0)
        parse-int (fn [s] (try (Integer/parseInt (str/trim s)) (catch Exception _ 0)))]
    (loop [i 0]
      (when (< i (count lines))
        (let [line (str/trim (nth lines i))]
          (cond
            (str/blank? line) (recur (inc i))
            (and (str/includes? line "x") (str/includes? line ":"))
            (let [[dim counts-str] (str/split line #":")
                  [w h] (map parse-int (str/split (str/trim dim) #"x"))
                  counts (str/split (str/trim counts-str) #"\s+")
                  [to-fit-raw total-req-area]
                  (loop [idx 0 qs counts acc [] tr 0]
                    (if (or (>= idx (count @shapes-ref)) (empty? qs))
                      [acc tr]
                      (let [q (parse-int (first qs))
                            sh (nth @shapes-ref idx)]
                        (recur (inc idx) (rest qs) (into acc (repeat q sh)) (+ tr (* q (:area sh)))))))
                  to-fit (->> to-fit-raw
                              (sort-by :area >)
                              (mapv (fn [sh]
                                      (update sh :orientations
                                              (fn [ors]
                                                (mapv (fn [o] (assoc o :off (int-array (map (fn [[px py]] (+ (* py w) px)) (:pts o))))) ors))))))]
              (if (solve 0 (boolean-array (* w h)) w h to-fit total-req-area (* w h))
                (swap! total inc))
              (recur (inc i)))
            (str/ends-with? line ":")
            (let [[grid-rows next-i]
                  (loop [j (inc i) acc []]
                    (if (>= j (count lines)) [acc j]
                      (let [l (str/trim (nth lines j))]
                        (if (or (str/blank? l) (str/includes? l ":")) [acc j]
                          (recur (inc j) (conj acc l))))))
                  pts (for [r (range (count grid-rows))
                            c (range (count (nth grid-rows r)))
                            :when (= \# (nth (nth grid-rows r) c))]
                        [c r])]
              (when (seq pts)
                (swap! shapes-ref conj {:area (count pts) :orientations (get-orientations pts)}))
              (recur next-i))
            :else (recur (inc i))))))
    (println @total)))

(-main)
