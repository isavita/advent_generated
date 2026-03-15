
(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input []
  (let [content (slurp "input.txt")]
    (if (str/blank? content)
      []
      (str/split-lines content))))

(defn separator? [lines x]
  (every? (fn [line]
            (or (>= x (count line))
                (Character/isWhitespace (nth line x))))
          lines))

(defn process-block [lines start end]
  (let [nums (atom [])
        operator (atom "*")]
    (doseq [c (range start (inc end))]
      (let [sb (StringBuilder.)
            has-digits (atom false)]
        (doseq [r (range (count lines))]
          (when (< c (count (nth lines r)))
            (let [ch (nth (nth lines r) c)]
              (when (Character/isDigit ch)
                (.append sb ch)
                (reset! has-digits true))
              (when (or (= ch \+) (= ch \*))
                (reset! operator (str ch))))))
        (when @has-digits
          (swap! nums conj (bigint (str sb))))))
    (if (empty? @nums)
      0N
      (if (= @operator "*")
        (reduce * 1N @nums)
        (reduce + 0N @nums)))))

(defn solve []
  (let [lines (read-input)]
    (if (or (empty? lines) (and (= (count lines) 1) (str/blank? (first lines))))
      (println "Grand total: 0")
      (let [max-width (apply max (map count lines))
            is-sep (vec (for [x (range max-width)]
                          (separator? lines x)))
            grand-total (atom 0N)
            in-block (atom false)
            start-col (atom 0)]
        (doseq [x (range max-width)]
          (if-not (nth is-sep x)
            (when-not @in-block
              (reset! in-block true)
              (reset! start-col x))
            (when @in-block
              (swap! grand-total + (process-block lines @start-col (dec x)))
              (reset! in-block false))))
        (when @in-block
          (swap! grand-total + (process-block lines @start-col (dec max-width))))
        (println (str "Grand total: " @grand-total))))))

(solve)
