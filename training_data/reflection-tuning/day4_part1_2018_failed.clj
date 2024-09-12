(ns guard-sleep-analysis
  (:require [clojure.string :as str]))

(defn parse-log-entry [entry]
  (let [[_ date time event] (re-find #"\[(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2})\] (.+)" entry)]
    {:date date
     :time time
     :event event}))

(defn process-logs [logs]
  (loop [entries (map parse-log-entry logs)
         current-guard nil
         sleep-start nil
         guard-sleep-times {}]
    (if-let [{:keys [time event]} (first entries)]
      (cond
        (str/starts-with? event "Guard")
        (let [guard-id (Integer/parseInt (re-find #"\d+" event))]
          (recur (rest entries) guard-id nil guard-sleep-times))

        (= event "falls asleep")
        (recur (rest entries) current-guard time guard-sleep-times)

        (= event "wakes up")
        (let [start-minute (Integer/parseInt (subs sleep-start 3))
              end-minute (Integer/parseInt (subs time 3))
              sleep-range (range start-minute end-minute)
              updated-times (update guard-sleep-times current-guard
                                    (fnil #(merge-with + % (frequencies sleep-range)) {}))]
          (recur (rest entries) current-guard nil updated-times))

        :else
        (recur (rest entries) current-guard sleep-start guard-sleep-times))
      guard-sleep-times)))

(defn find-sleepiest-guard [guard-sleep-times]
  (apply max-key (fn [[_ times]] (apply + (vals times))) guard-sleep-times))

(defn find-sleepiest-minute [sleep-times]
  (apply max-key val sleep-times))

(defn solve [logs]
  (let [guard-sleep-times (process-logs logs)
        [sleepiest-guard-id sleep-times] (find-sleepiest-guard guard-sleep-times)
        [sleepiest-minute _] (find-sleepiest-minute sleep-times)]
    (* sleepiest-guard-id sleepiest-minute)))

(defn -main [& args]
  (let [logs (str/split-lines (slurp (first args)))]
    (println (solve logs))))
