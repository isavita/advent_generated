
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:const top    [0 -1])
(def ^:const right  [1 0])
(def ^:const bottom [0 1])
(def ^:const left   [-1 0])

(def tile->pipe
  {\| #{top bottom}
   \- #{left right}
   \J #{top left}
   \L #{top right}
   \7 #{bottom left}
   \F #{bottom right}})

(defn add [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(defn opp  [[x y]]       [(- x) (- y)])

(defn pipe-at [[x y] grid]
  (let [c (get-in grid [y x])]
    (tile->pipe c)))

(defn start-pipe [pos grid]
  (into #{}
        (for [dir [top right bottom left]
              :let [npos (add pos dir)
                    ndir (opp dir)
                    pipe (pipe-at npos grid)]
              :when (contains? pipe ndir)]
          dir)))

(defn find-start [grid]
  (first
   (for [y (range (count grid))
         x (range (count (first grid)))
         :when (= \S (get-in grid [y x]))]
     [x y])))

(defn path [grid]
  (let [start (find-start grid)
        spipe (start-pipe start grid)
        dir   (first (seq spipe))]
    (loop [pos start
           dir dir
           visited []]
      (let [npos (add pos dir)
            visited (conj visited pos)]
        (if (= npos start)
          visited
          (let [npipe (pipe-at npos grid)
                ndir  (first (filter #(not= % (opp dir)) npipe))]
            (recur npos ndir visited)))))))

(defn solve [grid]
  (let [p (path grid)]
    (/ (count p) 2)))

(defn -main [& _]
  (let [grid (str/split-lines (slurp "input.txt"))]
    (println (solve grid))))

(-main)
