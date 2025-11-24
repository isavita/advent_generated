
(ns robot
  (:require [clojure.string :as str]))

(defn read-input [file]
  (let [lines (str/split-lines (slurp file))]
    (loop [grid [] moves [] lines lines]
      (if (empty? lines)
        [grid (apply str moves)]
        (let [line (first lines)]
          (if (str/includes? line "#")
            (recur (conj grid (vec line)) moves (rest lines))
            (recur grid (conj moves line) (rest lines))))))))

(defn find-robot [grid]
  (first (for [r (range (count grid))
               c (range (count (grid r)))
               :when (= (get-in grid [r c]) \@)]
           [r c])))

(defn move-dir [d]
  (case d
    \^ [-1 0]
    \v [1 0]
    \< [0 -1]
    \> [0 1]))

(defn push-boxes [grid r c dr dc]
  (let [nr (+ r dr) nc (+ c dc)]
    (cond
      (= (get-in grid [nr nc]) \#) nil
      (= (get-in grid [nr nc]) \O)
      (if-let [g (push-boxes grid nr nc dr dc)]
        (recur g r c dr dc)
        nil)
      (= (get-in grid [nr nc]) \.)
      (-> grid
          (assoc-in [nr nc] \O)
          (assoc-in [r c] \.))
      :else nil)))

(defn move-robot [grid r c d]
  (let [[dr dc] (move-dir d)
        nr (+ r dr) nc (+ c dc)]
    (cond
      (= (get-in grid [nr nc]) \#) [grid r c]
      (= (get-in grid [nr nc]) \O)
      (if-let [g (push-boxes grid nr nc dr dc)]
        [(-> g (assoc-in [r c] \.) (assoc-in [nr nc] \@)) nr nc]
        [grid r c])
      :else
      [(-> grid (assoc-in [r c] \.) (assoc-in [nr nc] \@)) nr nc])))

(defn gps-sum [grid]
  (reduce + (for [r (range (count grid))
                  c (range (count (grid r)))
                  :when (= (get-in grid [r c]) \O)]
              (+ (* r 100) c))))

(defn -main []
  (let [[grid moves] (read-input "input.txt")
        [grid r c] (loop [grid grid r (first (find-robot grid)) c (second (find-robot grid)) [m & ms] moves]
                      (if (nil? m)
                        [grid r c]
                        (let [[g nr nc] (move-robot grid r c m)]
                          (recur g nr nc ms))))]
    (println (gps-sum grid))))

(-main)
