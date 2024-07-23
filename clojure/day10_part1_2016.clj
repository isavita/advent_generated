
(ns balance-bots
  (:require [clojure.string :as str]))

(defn parse-instructions [lines]
  (let [value-regex #"value (\d+) goes to bot (\d+)"
        bot-regex #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)"]
    (reduce (fn [acc line]
              (cond
                (re-matches value-regex line)
                (let [[_ value bot] (re-find value-regex line)]
                  (update acc (Integer. bot) #(conj (or % []) (Integer. value))))

                (re-matches bot-regex line)
                (let [[_ bot low-type low-target high-type high-target] (re-find bot-regex line)]
                  (assoc-in acc [::bots (Integer. bot)] {:low {:type low-type :target (Integer. low-target)}
                                                          :high {:type high-type :target (Integer. high-target)}}))
                :else acc)))
            {::bots {} ::values {}} lines)))

(defn process-bots [instructions]
  (let [bots (atom (:bots instructions))
        values (atom (:values instructions))]
    (loop []
      (let [ready-bots (filter #(= 2 (count (second %))) @bots)]
        (if (empty? ready-bots)
          @bots
          (do
            (doseq [[bot chips] ready-bots]
              (let [low-chip (apply min chips)
                    high-chip (apply max chips)
                    bot-instructions (get @bots bot)]
                (when (and (= low-chip 17) (= high-chip 61))
                  (println "Bot responsible for comparing 61 and 17 is:" bot))
                (swap! bots update bot #(disj % low-chip high-chip))
                (swap! bots update (-> bot-instructions :low :target) #(conj (or % []) low-chip))
                (swap! bots update (-> bot-instructions :high :target) #(conj (or % []) high-chip))))
            (recur)))))))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        instructions (parse-instructions lines)]
    (process-bots instructions)))

(-main)
