
(ns fresh-ids
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-range [line]
  (let [[_ lo hi] (re-matches #"(\d+)-(\d+)" line)]
    [(Long/parseUnsignedLong lo)
     (Long/parseUnsignedLong hi)]))

(defn merge-ranges [ranges]
  (reduce (fn [acc [lo hi]]
            (if-let [[[plo phi] & more] (seq acc)]
              (if (<= lo (inc phi))
                (cons [(min plo lo) (max phi hi)] more)
                (cons [lo hi] acc))
              [[lo hi]]))
          []
          (sort-by first ranges)))

(defn -main [& _]
  (with-open [rdr (io/reader "input.txt")]
    (let [ranges (->> (line-seq rdr)
                      (take-while (complement str/blank?))
                      (map parse-range))
          total  (reduce + (map (fn [[lo hi]] (inc (- hi lo))) (merge-ranges ranges)))]
      (println total))))

(-main)
