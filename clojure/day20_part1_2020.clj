
(ns solve
  (:require [clojure.string :as str]))

(defn slurp-lines [f]
  (str/split-lines (slurp f)))

(defn parse-tiles [lines]
  (loop [lines lines
         id nil
         rows []
         tiles []]
    (cond
      (empty? lines)
      (cond-> tiles id (conj {:id id :data rows}))

      (str/starts-with? (first lines) "Tile")
      (let [new-id (Long/parseLong (second (re-matches #"Tile (\d+):" (first lines))))]
        (recur (rest lines) new-id [] (if id (conj tiles {:id id :data rows}) tiles)))

      (str/blank? (first lines))
      (recur (rest lines) id rows tiles)

      :else
      (recur (rest lines) id (conj rows (first lines)) tiles))))

(defn border [data idx]
  (case idx
    0 (first data)
    1 (last data)
    2 (apply str (map first data))
    3 (apply str (map last data))))

(defn all-borders [tile]
  (for [i (range 4)
        s [(border (:data tile) i) (str/join (reverse (border (:data tile) i)))]]
    {:border s :id (:id tile) :side i}))

(defn unique-matches [borders]
  (let [grouped (group-by :border borders)]
    (into {} (for [[k v] grouped]
               [k (count v)]))))

(defn corner-ids [tiles]
  (let [borders (mapcat all-borders tiles)
        counts (unique-matches borders)
        side-matches (fn [id] (frequencies
                                 (for [b (filter #(= (:id %) id) borders)
                                       :let [c (get counts (:border b))]
                                       :when (= c 1)]
                                   (:side b))))]
    (for [t tiles
          :let [sides (side-matches (:id t))]
          :when (= (count sides) 2)]
      (:id t))))

(defn -main []
  (let [tiles (parse-tiles (slurp-lines "input.txt"))
        corners (corner-ids tiles)]
    (println (reduce * (take 4 corners)))))

(-main)
