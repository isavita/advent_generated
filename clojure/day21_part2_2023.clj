
(ns garden
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-data [lines]
  (let [garden (atom #{}), start (atom nil)]
    (doseq [[y line] (map-indexed vector lines)
            [x c] (map-indexed vector line)]
      (when (not= c \#) (swap! garden conj [(mod x (count (first lines))) (mod y (count lines))]))
      (when (= c \S) (reset! start [(mod x (count (first lines))) (mod y (count lines))])))
    [@garden @start]))

(defn next-states [garden max-size states]
  (into #{}
        (for [[x y] states
              [dx dy] [[1 0] [-1 0] [0 1] [0 -1]]
              :let [nx (mod (+ x dx) max-size)
                    ny (mod (+ y dy) max-size)]
              :when (garden [nx ny])]
          [(+ x dx) (+ y dy)])))

(defn calc-ends [garden start steps max-size]
  (loop [states #{start}, step 0, done []]
    (cond
      (= (count done) 3) done
      (and (< step (* 3 max-size)) (= (mod step max-size) (quot (dec max-size) 2)))
      (recur (next-states garden max-size states) (inc step) (conj done (count states)))
      :else (recur (next-states garden max-size states) (inc step) done))))

(defn quadratic [n a b c]
  (+ a (* n (- b a)) (* n (dec n) (/ (+ c (* -2 b) a) 2))))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        [garden start] (parse-data lines)
        max-size (count lines)
        [a b c] (calc-ends garden start 26501365 max-size)]
    (println (quadratic (quot 26501365 max-size) a b c))))

(-main)
