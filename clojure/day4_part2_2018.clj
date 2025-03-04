
(ns guard-duty
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-line [line]
  (let [[_ timestamp event] (re-matches #"\[(.*?)\] (.*)" line)]
    {:timestamp timestamp
     :event event}))

(defn parse-guard-id [event]
  (when (str/starts-with? event "Guard")
    (let [[_ id] (re-matches #"Guard #(\d+) begins shift" event)]
      (edn/read-string id))))

(defn process-records [records]
  (loop [records records
         sleep-data {}
         current-guard nil
         sleep-start nil]
    (if (empty? records)
      sleep-data
      (let [{:keys [timestamp event]} (first records)]
        (if-let [guard-id (parse-guard-id event)]
          (recur (rest records)
                 sleep-data
                 guard-id
                 nil)
          (cond
            (str/starts-with? event "falls asleep")
            (recur (rest records)
                   sleep-data
                   current-guard
                   (Integer/parseInt (subs timestamp 14 16)))
            (str/starts-with? event "wakes up")
            (let [wake-time (Integer/parseInt (subs timestamp 14 16))
                  sleep-duration (- wake-time sleep-start)
                  sleep-minutes (range sleep-start wake-time)]
              (recur (rest records)
                     (update sleep-data current-guard
                             (fn [existing-minutes]
                               (concat (or existing-minutes []) sleep-minutes)))
                     current-guard
                     nil)
              )
            :else (recur (rest records) sleep-data current-guard sleep-start)))))))

(defn solve-part1 [sleep-data]
  (let [sleepiest-guard (apply max-key (comp count val) sleep-data)
        minute-counts (frequencies (val sleepiest-guard))
        sleepiest-minute (apply max-key val minute-counts)]
    (* (key sleepiest-guard) (key sleepiest-minute))))

(defn solve-part2 [sleep-data]
  (let [minute-frequencies (reduce (fn [acc [guard minutes]]
                                     (let [freqs (frequencies minutes)]
                                       (reduce (fn [acc [minute count]]
                                                 (assoc acc [guard minute] count))
                                               acc
                                               freqs)))
                                   {} sleep-data)
        [ [sleepiest-guard sleepiest-minute] _] (apply max-key val minute-frequencies)]
    (* sleepiest-guard sleepiest-minute)))

(defn main []
  (let [records (->> "input.txt"
                       slurp
                       str/split-lines
                       (map parse-line)
                       (sort-by :timestamp)
                       (vec))
        sleep-data (process-records records)]
    (println "Part 1:" (solve-part1 sleep-data))
    (println "Part 2:" (solve-part2 sleep-data))))

(main)
