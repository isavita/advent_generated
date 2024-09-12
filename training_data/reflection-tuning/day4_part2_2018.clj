(ns day4-solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (let [[_ year month day hour minute event] (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)" line)]
    {:timestamp (str year month day hour minute)
     :minute (Integer/parseInt minute)
     :event event}))

(defn process-events [events]
  (loop [events events
         current-guard nil
         sleep-start nil
         guard-sleep-patterns {}]
    (if-let [{:keys [minute event]} (first events)]
      (cond
        (str/starts-with? event "Guard")
        (recur (rest events) (Integer/parseInt (re-find #"\d+" event)) nil guard-sleep-patterns)
        
        (= event "falls asleep")
        (recur (rest events) current-guard minute guard-sleep-patterns)
        
        (= event "wakes up")
        (let [sleep-range (range sleep-start minute)
              updated-patterns (update guard-sleep-patterns current-guard
                                       (fnil #(reduce (fn [acc m] (update acc m (fnil inc 0))) % sleep-range)
                                             (vec (repeat 60 0))))]
          (recur (rest events) current-guard nil updated-patterns))
        
        :else (recur (rest events) current-guard sleep-start guard-sleep-patterns))
      guard-sleep-patterns)))

(defn find-sleepiest-guard [guard-sleep-patterns]
  (->> guard-sleep-patterns
       (map (fn [[guard pattern]] [guard (apply + pattern)]))
       (apply max-key second)
       first))

(defn find-sleepiest-minute [sleep-pattern]
  (->> sleep-pattern
       (map-indexed vector)
       (apply max-key second)
       first))

(defn solve-part1 [guard-sleep-patterns]
  (let [sleepiest-guard (find-sleepiest-guard guard-sleep-patterns)
        sleepiest-minute (find-sleepiest-minute (get guard-sleep-patterns sleepiest-guard))]
    (* sleepiest-guard sleepiest-minute)))

(defn solve-part2 [guard-sleep-patterns]
  (->> guard-sleep-patterns
       (mapcat (fn [[guard pattern]]
                 (map (fn [minute count] [guard minute count]) (range) pattern)))
       (apply max-key last)
       (take 2)
       (apply *)))

(defn solve-day4 []
  (let [input (->> (io/reader "input.txt")
                   line-seq
                   (map parse-line)
                   (sort-by :timestamp))
        guard-sleep-patterns (process-events input)]
    (println "Part 1:" (solve-part1 guard-sleep-patterns))
    (println "Part 2:" (solve-part2 guard-sleep-patterns))))

(solve-day4)
