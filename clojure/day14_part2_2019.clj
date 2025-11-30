
(ns space-stoichiometry
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-reactions [input]
  (reduce
   (fn [acc line]
     (let [[inputs output] (str/split line #" => ")
           [_ out-qty out-chem] (re-matches #"(\d+) (\w+)" output)
           inputs (reduce
                   (fn [acc in]
                     (let [[_ in-qty in-chem] (re-matches #"(\d+) (\w+)" in)]
                       (assoc acc in-chem (Long/parseLong in-qty))))
                   {} (str/split inputs #", "))]
       (assoc acc out-chem {:inputs inputs :output-qty (Long/parseLong out-qty)})))
   {} (str/split-lines (str/trim input))))

(defn ore-for-fuel [reactions fuel-amount]
  (loop [needed {"FUEL" fuel-amount}
         ore 0
         inventory {}]
    (if (empty? needed)
      ore
      (let [[chem qty] (first needed)
            needed (dissoc needed chem)]
        (cond
          (= chem "ORE")
          (recur needed (+ ore qty) inventory)

          (>= (get inventory chem 0) qty)
          (recur needed ore (update inventory chem - qty))

          :else
          (let [inv-qty (get inventory chem 0)
                qty (- qty inv-qty)
                inventory (assoc inventory chem 0)
                {:keys [inputs output-qty]} (get reactions chem)
                multiplier (long (Math/ceil (/ qty output-qty)))
                produced (* output-qty multiplier)
                leftover (- produced qty)]
            (recur (reduce (fn [acc [in-chem in-qty]]
                             (update acc in-chem (fnil + 0) (* in-qty multiplier)))
                           needed inputs)
                   ore
                   (if (pos? leftover)
                     (update inventory chem (fnil + 0) leftover)
                     inventory))))))))

(defn max-fuel [reactions total-ore]
  (let [low 0
        high total-ore]
    (loop [low low high high max-fuel 0]
      (if (> low high)
        max-fuel
        (let [mid (long (/ (+ low high) 2))
              ore-needed (ore-for-fuel reactions mid)]
          (if (<= ore-needed total-ore)
            (recur (inc mid) high mid)
            (recur low (dec mid) max-fuel)))))))

(defn -main [& _]
  (let [input (slurp "input.txt")
        reactions (parse-reactions input)
        ore-for-one (ore-for-fuel reactions 1)
        total-ore 1000000000000
        max-fuel (max-fuel reactions total-ore)]
    (println "Part 1:" ore-for-one)
    (println "Part 2:" max-fuel)))

(-main)
