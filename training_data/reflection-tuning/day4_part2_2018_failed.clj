(ns day4.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (let [[_ year month day hour minute action id] (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*?)(?: #(\d+))?" line)]
    {:datetime (str year "-" month "-" day " " hour ":" minute)
     :minute (Integer/parseInt minute)
     :action action
     :id (when id (Integer/parseInt id))}))

(defn process-entries [entries]
  (reduce (fn [{:keys [current-guard] :as acc} {:keys [action id minute]}]
            (case action
              "Guard" (assoc acc :current-guard id)
              "falls asleep" (assoc-in acc [:sleep-patterns current-guard :start] minute)
              "wakes up" (update-in acc [:sleep-patterns current-guard :minutes]
                                    #(reduce (fn [m i] (update m i (fnil inc 0)))
                                             %
                                             (range (get-in acc [:sleep-patterns current-guard :start]) minute)))))
          {:sleep-patterns {}}
          entries))

(defn find-sleepiest-guard [sleep-patterns]
  (->> sleep-patterns
       (map (fn [[id pattern]] [id (apply + (vals (:minutes pattern)))]))
       (apply max-key second)))

(defn find-sleepiest-minute [minutes]
  (apply max-key val minutes))

(defn find-most-frequent-minute [sleep-patterns]
  (->> sleep-patterns
       (mapcat (fn [[id pattern]]
                 (map (fn [[minute count]] [id minute count]) (:minutes pattern))))
       (apply max-key last)))

(defn solve-puzzle []
  (let [entries (->> (io/reader "input.txt")
                     line-seq
                     (map parse-line)
                     (sort-by :datetime))
        {:keys [sleep-patterns]} (process-entries entries)
        [guard1 _] (find-sleepiest-guard sleep-patterns)
        [minute1 _] (find-sleepiest-minute (get-in sleep-patterns [guard1 :minutes]))
        [guard2 minute2 _] (find-most-frequent-minute sleep-patterns)]
    (println "Part 1:" (* guard1 minute1))
    (println "Part 2:" (* guard2 minute2))))

(solve-puzzle)
