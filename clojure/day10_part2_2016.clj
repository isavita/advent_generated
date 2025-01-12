
(ns bot-simulation
  (:require [clojure.string :as str]))

(defn parse-input [lines]
  (let [value-regex #"value (\d+) goes to (bot \d+)"
        gives-regex #"(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)"]
    (reduce (fn [acc line]
              (cond
                (re-matches value-regex line)
                (let [[_ value bot-id] (re-matches value-regex line)]
                  (update-in acc [:bots bot-id :chips] conj (Integer/parseInt value)))

                (re-matches gives-regex line)
                (let [[_ bot-id low-to high-to] (re-matches gives-regex line)]
                  (-> acc
                      (assoc-in [:bots bot-id :low-to] low-to)
                      (assoc-in [:bots bot-id :high-to] high-to)))

                :else acc))
            {:bots {} :outputs {}} lines)))

(defn give-chip [state target value]
  (if (str/starts-with? target "bot")
    (update-in state [:bots target :chips] conj value)
    (assoc-in state [:outputs target] value)))

(defn process-bots [state]
  (loop [s state]
    (let [action (reduce-kv (fn [acc bot-id bot]
                              (if (= (count (:chips bot)) 2)
                                (let [[low high] (sort (:chips bot))]
                                  (-> acc
                                      (assoc-in [:bots bot-id :chips] [])
                                      (give-chip (:low-to bot) low)
                                      (give-chip (:high-to bot) high)))
                                acc))
                            s (:bots s))]
      (if (not= s action)
        (recur action)
        s))))

(defn solve []
  (let [lines (str/split-lines (slurp "input.txt"))
        initial-state (parse-input lines)
        final-state (process-bots initial-state)]
    (* (get-in final-state [:outputs "output 0"])
       (get-in final-state [:outputs "output 1"])
       (get-in final-state [:outputs "output 2"]))))

(println (solve))
