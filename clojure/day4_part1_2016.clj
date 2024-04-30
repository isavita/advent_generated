(ns day4
  (:require [clojure.string :as str]
             [clojure.data :as data]))

(defn parse-room [line]
  (let [[_ name sector-id checksum] (re-matches #"(.*)-(\d+)\[(.*)\]" line)]
    {:name (str/split name #"-")
     :sector-id (Integer/parseInt sector-id)
     :checksum checksum}))

(defn most-common-letters [letters]
  (let [freqs (frequencies (apply concat letters))]
    (->> freqs
         (sort-by val >)
         (partition-by val)
         (map #(sort-by first %))
         (apply concat)
         (take 5)
         (map first)
         (str/join))))

(defn is-real-room? [room]
  (= (:checksum room) (most-common-letters (:name room))))

(defn sum-of-sector-ids [rooms]
  (->> rooms
       (filter is-real-room?)
       (map :sector-id)
       (reduce +)))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [rooms (doall (map parse-room (line-seq rdr)))]
      (println "Sum of sector IDs of real rooms:" (sum-of-sector-ids rooms)))))

(-main)