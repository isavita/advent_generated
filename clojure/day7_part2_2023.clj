
(ns camel-cards
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn card-val [c]
  (case c
    \T 10 \J 1 \Q 12 \K 13 \A 14
    (Long/parseUnsignedLong (str c))))

(defn hand-rank [cards]
  (let [cnt (frequencies cards)
        js  (get cnt \J 0)
        cnt (dissoc cnt \J)
        cnt (if (empty? cnt) {\J js}
              (let [[m v] (apply max-key val cnt)]
                (assoc cnt m (+ v js))))]
    (condp = (sort > (vals cnt))
      [5]   6
      [4 1] 5
      [3 2] 4
      [3 1 1] 3
      [2 2 1] 2
      [2 1 1 1] 1
      0)))

(defn hand-key [cards]
  [(hand-rank cards)
   (mapv card-val cards)])

(defn parse-line [line]
  (let [[cards bid] (str/split line #"\s+")]
    [(seq cards) (Long/parseUnsignedLong bid)]))

(defn -main [& _]
  (let [hands (with-open [rdr (io/reader "input.txt")]
                (->> (line-seq rdr)
                     (keep (fn [l] (when (>= (count l) 6) l)))
                     (mapv parse-line)))
        ranked (sort-by (comp hand-key first) hands)]
    (println (transduce (map-indexed (fn [i [_ b]] (* b (inc i)))) + ranked))))

(-main)
