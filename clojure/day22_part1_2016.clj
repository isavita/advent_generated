
(ns viable-pairs
  (:require [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn read-nodes [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> (line-seq rdr)
         (map #(re-find #"node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%" %))
         (filter identity)
         (map #(map parse-int (rest %)))
         (map (fn [[used avail]] {:used used :avail avail}))
         (vec))))

(defn count-viable-pairs [nodes]
  (let [n (count nodes)]
    (loop [i 0 count 0]
      (if (= i n)
        count
        (let [a (nth nodes i)]
          (recur (inc i)
                 (loop [j 0 count count]
                   (if (= j n)
                     count
                     (let [b (nth nodes j)]
                       (recur (inc j)
                              (if (and (not= i j)
                                       (> (:used a) 0)
                                       (<= (:used a) (:avail b)))
                                (inc count)
                                count)))))))))))

(let [nodes (read-nodes "input.txt")]
  (println (count-viable-pairs nodes)))
