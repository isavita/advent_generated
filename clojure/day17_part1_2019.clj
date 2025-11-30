
(ns day17
  (:require [clojure.string :as str]))

(defn decode [n]
  [(mod n 100) (map #(mod (quot n %) 10) [100 1000 10000])])

(defn machine [program in-stream]
  (let [data (atom (into {} (map-indexed vector program)))
        ip (atom 0)
        relbase (atom 0)
        out-stream (atom [])
        get-val (fn [i mode]
                  (case mode
                    0 (get @data (get @data i) 0)
                    1 (get @data i 0)
                    2 (get @data (+ @relbase (get @data i)) 0)))
        set-val (fn [i mode value]
                  (case mode
                    0 (swap! data assoc (get @data i) value)
                    2 (swap! data assoc (+ @relbase (get @data i)) value)))]
    (letfn [(step []
              (let [[op modes] (decode (get @data @ip 0))]
                (case op
                  1 (do (set-val (+ @ip 3) (nth modes 2)
                                 (+ (get-val (+ @ip 1) (first modes))
                                    (get-val (+ @ip 2) (second modes))))
                        (swap! ip + 4))
                  2 (do (set-val (+ @ip 3) (nth modes 2)
                                 (* (get-val (+ @ip 1) (first modes))
                                    (get-val (+ @ip 2) (second modes))))
                        (swap! ip + 4))
                  3 (do (set-val (+ @ip 1) (first modes) (first @in-stream))
                        (swap! in-stream rest)
                        (swap! ip + 2))
                  4 (do (swap! out-stream conj (get-val (+ @ip 1) (first modes)))
                        (swap! ip + 2))
                  5 (if (not= (get-val (+ @ip 1) (first modes)) 0)
                        (reset! ip (get-val (+ @ip 2) (second modes)))
                        (swap! ip + 3))
                  6 (if (= (get-val (+ @ip 1) (first modes)) 0)
                        (reset! ip (get-val (+ @ip 2) (second modes)))
                        (swap! ip + 3))
                  7 (do (set-val (+ @ip 3) (nth modes 2)
                                 (if (< (get-val (+ @ip 1) (first modes))
                                        (get-val (+ @ip 2) (second modes))) 1 0))
                        (swap! ip + 4))
                  8 (do (set-val (+ @ip 3) (nth modes 2)
                                 (if (= (get-val (+ @ip 1) (first modes))
                                        (get-val (+ @ip 2) (second modes))) 1 0))
                        (swap! ip + 4))
                  9 (do (swap! relbase + (get-val (+ @ip 1) (first modes)))
                        (swap! ip + 2))
                  99 false)))]
      (while (step))
      @out-stream)))

(defn parse [program]
  (let [out (machine program [])
        grid (atom {})
        robot (atom nil)
        dir (atom 0)
        x (atom 0)
        y (atom 0)]
    (doseq [o out]
      (let [c (char o)]
        (cond
          (= c \newline) (do (swap! y inc) (reset! x 0))
          (some #(= c %) [\^ \v \< \>])
          (do (reset! robot [@x @y])
              (reset! dir (.indexOf "^>v<" (str c)))
              (swap! grid assoc [@x @y] \#)
              (swap! x inc))
          (= c \#) (do (swap! grid assoc [@x @y] \#)
                       (swap! x inc))
          :else (swap! x inc))))
    [@grid @robot @dir]))

(defn sum-align [grid]
  (transduce
    (comp (filter (fn [[x y]]
                    (every? #(get grid [(+ x (first %)) (+ y (second %))]) [[0 1] [0 -1] [1 0] [-1 0]])))
          (map (fn [[x y]] (* x y))))
    + 0 (keys grid)))

(defn -main []
  (let [program (mapv #(Long/parseLong %) (str/split (str/trim (slurp "input.txt")) #","))
        [scaffolding _ _] (parse program)]
    (println (sum-align scaffolding))))

(-main)
