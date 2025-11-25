
(ns day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-record [line]
  (let [[_ ts-str action guard-id]
        (re-matches #"\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (.*)" line)
        [_ id] (re-matches #"Guard #(\d+).*" action)]
    {:ts       (java.time.LocalDateTime/parse ts-str
                       (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))
     :action   (cond id "begins shift"
                     (str/includes? action "falls") "falls asleep"
                     :else "wakes up")
     :guard-id (if id (Long/parseLong id) -1)}))

(defn read-input [file]
  (->> file io/reader line-seq (map parse-record)
       (sort-by :ts)))

(defn sleep-map [records]
  (loop [records records guard nil asleep nil acc {}]
    (if (empty? records) acc
      (let [{:keys [action guard-id ts]} (first records)]
        (case action
          "begins shift" (recur (rest records) guard-id nil acc)
          "falls asleep" (recur (rest records) guard ts acc)
          "wakes up" (let [start (.getMinute asleep) end (.getMinute ts)
                           m (get acc guard (vec (repeat 60 0)))]
                       (recur (rest records) guard nil
                        (assoc acc guard
                         (vec (for [i (range 60)]
                               (if (and (>= i start) (< i end))
                                 (inc (nth m i)) (nth m i))))))))))))

(defn sleepiest [sm]
  (let [guard (apply max-key #(reduce + (sm %)) (keys sm))
        mins  (sm guard)
        minute (apply max-key #(nth mins %) (range 60))]
    (* guard minute)))

(-> (read-input "input.txt") sleep-map sleepiest println)
