
(ns turing
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-blueprint [file]
  (let [lines (line-seq (io/reader file))
        initial-state (nth (first lines) (- (count (first lines)) 2))
        steps (Long/parseLong (re-find #"\d+" (second lines)))
        states (atom {})]
    (doseq [block (partition-by empty? (drop 2 lines))
            :when (not (empty? (first block)))
            :let [state (nth (first block) (- (count (first block)) 2))]]
      (doseq [case (partition 4 (rest block))
              :let [val (Character/digit (nth (first case) (- (count (first case)) 2)) 10)
                    write-val (Character/digit (nth (second case) (- (count (second case)) 2)) 10)
                    move-dir (if (str/includes? (nth case 2) "left") -1 1)
                    next-state (nth (nth case 3) (- (count (nth case 3)) 2))]]
        (swap! states assoc-in [state val] [write-val move-dir next-state])))
    [initial-state steps @states]))

(defn run-turing [initial-state steps states]
  (let [tape (atom {})
        cursor (atom 0)
        state (atom initial-state)]
    (dotimes [_ steps]
      (let [current-val (get @tape @cursor 0)
            [write-val move-dir next-state] (get-in states [@state current-val])]
        (swap! tape assoc @cursor write-val)
        (swap! cursor + move-dir)
        (reset! state next-state)))
    (reduce + (vals @tape))))

(defn -main [& args]
  (let [[initial-state steps states] (parse-blueprint "input.txt")]
    (println (run-turing initial-state steps states))))

(-main)
