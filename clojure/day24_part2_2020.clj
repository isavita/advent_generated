(ns lobby-layout
  (:require [clojure.java.io :as io]))

(defn parse-directions [line]
  (loop [s line
         directions []]
    (if (empty? s)
      directions
      (let [dir (if (or (= (first s) \e) (= (first s) \w))
                  (str (first s))
                  (str (first s) (second s)))]
        (recur (subs s (count dir)) (conj directions dir))))))

(defn move [coord dir]
  (let [[x y] coord]
    (case dir
      "e" [(inc x) y]
      "w" [(dec x) y]
      "ne" [x (inc y)]
      "nw" [(dec x) (inc y)]
      "se" [(inc x) (dec y)]
      "sw" [x (dec y)])))

(defn flip-tile [black-tiles directions]
  (let [tile (reduce move [0 0] directions)]
    (if (contains? black-tiles tile)
      (disj black-tiles tile)
      (conj black-tiles tile))))

(defn adjacent-tiles [coord]
  (map #(move coord %) ["e" "w" "ne" "nw" "se" "sw"]))

(defn count-black-adjacent [black-tiles coord]
  (count (filter #(contains? black-tiles %) (adjacent-tiles coord))))

(defn simulate-day [black-tiles]
  (let [to-check (set (concat black-tiles (mapcat adjacent-tiles black-tiles)))]
    (reduce (fn [new-black-tiles tile]
              (let [black-adjacent (count-black-adjacent black-tiles tile)]
                (if (or (and (contains? black-tiles tile) (<= black-adjacent 2) (pos? black-adjacent))
                        (= black-adjacent 2))
                  (conj new-black-tiles tile)
                  new-black-tiles)))
            #{} to-check)))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [lines (line-seq r)
          directions (map parse-directions lines)
          black-tiles (reduce flip-tile #{} directions)]
      (loop [black-tiles black-tiles
             day 0]
        (if (= day 100)
          (println (count black-tiles))
          (recur (simulate-day black-tiles) (inc day)))))))

(-main)