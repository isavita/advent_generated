(require '[clojure.string :as str])

(defn valid-password? [s]
  (and (re-find #"(\d)\1" s)
       (apply <= (map int s))))

(defn count-valid-passwords [start end]
  (->> (range start (inc end))
       (map str)
       (filter valid-password?)
       count))

(defn solve-puzzle []
  (let [[start end] (map #(Integer/parseInt %)
                         (str/split (slurp "input.txt") #"-"))]
    (println (count-valid-passwords start end))))

(solve-puzzle)
